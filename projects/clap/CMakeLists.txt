# CLAP plugin framework. Each language adds its plugin with midi_langs_add_clap.
set(MIDI_LANGS_CLAP_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")

add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/pugl ${CMAKE_BINARY_DIR}/thirdparty/pugl)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/imgui ${CMAKE_BINARY_DIR}/thirdparty/imgui)

# Syntax highlighting: tree-sitter, its grammars, and the editor widget that shows the colours.
set(TREE_SITTER_DIR ${CMAKE_SOURCE_DIR}/thirdparty/tree-sitter)
add_library(tree_sitter STATIC ${TREE_SITTER_DIR}/lib/src/lib.c)
target_include_directories(tree_sitter PUBLIC ${TREE_SITTER_DIR}/lib/include PRIVATE ${TREE_SITTER_DIR}/lib/src)
set_target_properties(tree_sitter PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden)

# midi_langs_add_grammar(<name>): target tree_sitter_<name> and the generated header
# highlight_<name>.h, which declares tree_sitter_<name>() and holds the highlight query as
# kHighlightQuery_<name>.
set(MIDI_LANGS_HIGHLIGHT_DIR ${CMAKE_BINARY_DIR}/highlight CACHE INTERNAL "")
function(midi_langs_add_grammar name)
    set(dir ${CMAKE_SOURCE_DIR}/thirdparty/tree-sitter-grammars/${name})
    file(GLOB sources ${dir}/src/parser.c ${dir}/src/scanner.c)
    add_library(tree_sitter_${name} STATIC ${sources})
    target_include_directories(tree_sitter_${name} PRIVATE ${dir}/src)
    set_target_properties(tree_sitter_${name} PROPERTIES C_STANDARD 11 C_VISIBILITY_PRESET hidden)
    target_compile_options(tree_sitter_${name} PRIVATE -w) # generated code
    set(query ${dir}/queries/highlights.scm)
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS ${query})
    file(READ ${query} text)
    file(WRITE ${MIDI_LANGS_HIGHLIGHT_DIR}/highlight_${name}.h
        "// Generated from thirdparty/tree-sitter-grammars/${name}/queries/highlights.scm\n"
        "#pragma once\nstruct TSLanguage;\n"
        "extern \"C\" const TSLanguage* tree_sitter_${name}(void);\n"
        "static const char kHighlightQuery_${name}[] = R\"__tsq__(${text})__tsq__\";\n")
endfunction()
foreach(grammar lua python scheme haskell forth joy)
    midi_langs_add_grammar(${grammar})
endforeach()

# A code editor for Dear ImGui (C++20), patched so tree-sitter can colour it.
add_library(imgui_color_text_edit STATIC ${CMAKE_SOURCE_DIR}/thirdparty/imgui-color-text-edit/TextEditor.cpp)
target_include_directories(imgui_color_text_edit PUBLIC ${CMAKE_SOURCE_DIR}/thirdparty/imgui-color-text-edit)
target_link_libraries(imgui_color_text_edit PUBLIC imgui_gl3)
target_compile_features(imgui_color_text_edit PUBLIC cxx_std_20)
set_target_properties(imgui_color_text_edit PROPERTIES CXX_VISIBILITY_PRESET hidden)

# The parts that do not depend on the language or on pugl.
add_library(midi_langs_clap STATIC timeline.cpp editor_logic.cpp udp_server.cpp highlight.cpp)
target_include_directories(midi_langs_clap PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/clap/include
)
target_compile_features(midi_langs_clap PUBLIC cxx_std_17)
target_link_libraries(midi_langs_clap PUBLIC tree_sitter)
set_target_properties(midi_langs_clap PROPERTIES CXX_VISIBILITY_PRESET hidden)
if(WIN32)
    target_link_libraries(midi_langs_clap PUBLIC ws2_32)
endif()

if(BUILD_VST3)
    # clap-wrapper uses this target in place of a full CLAP SDK.
    add_library(clap INTERFACE)
    target_include_directories(clap INTERFACE ${CMAKE_SOURCE_DIR}/thirdparty/clap/include)
    set(VST3_SDK_ROOT ${CMAKE_SOURCE_DIR}/thirdparty/vst3sdk)
    add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/clap-wrapper ${CMAKE_BINARY_DIR}/thirdparty/clap-wrapper EXCLUDE_FROM_ALL)
    guarantee_vst3sdk() # here, where VST3_SDK_ROOT is set; the per-plugin calls then reuse it
endif()

# midi_langs_add_clap(<target> NAME <file name> CLASS_PREFIX <ObjC prefix>
#                     [GRAMMAR <tree-sitter grammar>]
#                     SOURCES <adapter sources> LIBRARIES <interpreter libraries>)
# Builds <build>/clap/<NAME>.clap and, with BUILD_VST3, <build>/vst3/<NAME>.vst3 (target
# <target>_vst3). The adapter defines kLanguage (language.hpp). Without GRAMMAR, the
# editor shows plain text.
function(midi_langs_add_clap target)
    cmake_parse_arguments(ARG "" "NAME;CLASS_PREFIX;GRAMMAR" "SOURCES;LIBRARIES" ${ARGN})

    # The plugin code, compiled once for both formats. Objects, not an archive: the linker
    # would drop plugin.cpp, which nothing references but the exported clap_entry.
    add_library(${target}_impl OBJECT
        ${MIDI_LANGS_CLAP_DIR}/plugin.cpp
        ${MIDI_LANGS_CLAP_DIR}/editor_imgui.cpp
        ${ARG_SOURCES}
    )
    target_link_libraries(${target}_impl PUBLIC midi_langs_clap imgui_color_text_edit ${ARG_LIBRARIES})
    if(ARG_GRAMMAR)
        target_link_libraries(${target}_impl PUBLIC tree_sitter_${ARG_GRAMMAR})
        target_include_directories(${target}_impl PRIVATE ${MIDI_LANGS_HIGHLIGHT_DIR})
        target_compile_definitions(${target}_impl PRIVATE
            MIDI_LANGS_HIGHLIGHT_HEADER="highlight_${ARG_GRAMMAR}.h"
            MIDI_LANGS_GRAMMAR=tree_sitter_${ARG_GRAMMAR}
            MIDI_LANGS_HIGHLIGHT_QUERY=kHighlightQuery_${ARG_GRAMMAR})
    endif()
    # pugl's headers only: each binary links its own pugl.
    target_include_directories(${target}_impl PRIVATE
        ${CMAKE_SOURCE_DIR}/projects/common # midi_clock.h
        ${PUGL_INCLUDE_DIR}
        ${CMAKE_SOURCE_DIR}/thirdparty/clap-wrapper/include) # wrapper_host.h, a header only
    target_compile_definitions(${target}_impl PRIVATE PUGL_STATIC MIDI_LANGS_VERSION="${CMAKE_PROJECT_VERSION}")
    set_target_properties(${target}_impl PROPERTIES CXX_VISIBILITY_PRESET hidden)
    if(APPLE)
        target_compile_definitions(${target}_impl PRIVATE GL_SILENCE_DEPRECATION)
    elseif(WIN32)
        target_compile_definitions(${target}_impl PRIVATE NOMINMAX) # <windows.h> via pugl/gl.h
    endif()

    # A pugl per binary: Objective-C classes share one namespace in the host process.
    midi_langs_add_pugl(${target}_pugl ${ARG_CLASS_PREFIX})
    add_library(${target} MODULE)
    target_link_libraries(${target} PRIVATE ${target}_impl ${target}_pugl)
    set_target_properties(${target} PROPERTIES
        OUTPUT_NAME ${ARG_NAME}
        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/clap
    )
    if(APPLE)
        target_link_options(${target} PRIVATE
            -exported_symbols_list ${CMAKE_SOURCE_DIR}/thirdparty/clap/src/macos-symbols.txt)
        set_target_properties(${target} PROPERTIES
            BUNDLE TRUE
            BUNDLE_EXTENSION clap
            MACOSX_BUNDLE_GUI_IDENTIFIER com.github.shakfu.midi-langs.${ARG_NAME}
            MACOSX_BUNDLE_BUNDLE_NAME ${ARG_NAME}
            MACOSX_BUNDLE_BUNDLE_VERSION ${CMAKE_PROJECT_VERSION}
            MACOSX_BUNDLE_SHORT_VERSION_STRING ${CMAKE_PROJECT_VERSION}
            MACOSX_BUNDLE_INFO_PLIST ${CMAKE_SOURCE_DIR}/thirdparty/clap/src/plugins.plist.in)
    else()
        set_target_properties(${target} PROPERTIES PREFIX "" SUFFIX ".clap")
        if(NOT WIN32)
            # Exports clap_entry only: the interpreter, libremidi, libuv and libstdc++'s
            # weak template instances stay out of the host's symbols.
            target_link_options(${target} PRIVATE
                -Wl,--exclude-libs,ALL
                -Wl,--version-script=${CMAKE_SOURCE_DIR}/thirdparty/clap/src/linux-my_plug.version)
        endif()
    endif()

    if(NOT BUILD_VST3)
        return()
    endif()
    # The VST3 links the plugin code and exports clap_entry, which clap-wrapper finds in
    # its own binary, so nothing is loaded at run time.
    midi_langs_add_pugl(${target}_vst3_pugl ${ARG_CLASS_PREFIX}Vst3)
    add_library(${target}_vst3 MODULE)
    target_link_libraries(${target}_vst3 PRIVATE ${target}_impl ${target}_vst3_pugl)
    target_add_vst3_wrapper(TARGET ${target}_vst3 OUTPUT_NAME ${ARG_NAME}
        BUNDLE_IDENTIFIER com.github.shakfu.midi-langs.${ARG_NAME}.vst3
        BUNDLE_VERSION ${CMAKE_PROJECT_VERSION}
        ASSET_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/vst3)
    if(APPLE)
        target_link_options(${target}_vst3 PRIVATE
            -exported_symbols_list ${MIDI_LANGS_CLAP_DIR}/vst3-macos-symbols.txt)
    elseif(NOT WIN32)
        # Not --exclude-libs: the VST3 entry points are in the SDK's archive.
        target_link_options(${target}_vst3 PRIVATE
            -Wl,--version-script=${MIDI_LANGS_CLAP_DIR}/vst3-linux.version)
    endif()
endfunction()

# Interactive host: a plugin's editor in a window, its MIDI to a MIDI port (`make clap-dev`).
midi_langs_add_pugl(clap_dev_host_pugl MidiLangsDevHost)
add_executable(clap_dev_host dev_host.cpp)
target_link_libraries(clap_dev_host PRIVATE clap_dev_host_pugl midi_open ${CMAKE_DL_LIBS})
target_include_directories(clap_dev_host PRIVATE ${CMAKE_SOURCE_DIR}/thirdparty/clap/include)
target_compile_features(clap_dev_host PRIVATE cxx_std_17)

# Headless host for the tests in tests/CMakeLists.txt.
add_executable(test_clap_host test_clap_host.cpp)
target_include_directories(test_clap_host PRIVATE ${CMAKE_SOURCE_DIR}/thirdparty/clap/include)
target_compile_features(test_clap_host PRIVATE cxx_std_17)
target_link_libraries(test_clap_host PRIVATE ${CMAKE_DL_LIBS})
if(WIN32)
    target_link_libraries(test_clap_host PRIVATE ws2_32)
endif()

add_executable(test_clap_units test_clap_units.cpp)
target_link_libraries(test_clap_units PRIVATE midi_langs_clap
    tree_sitter_lua tree_sitter_python tree_sitter_scheme tree_sitter_haskell tree_sitter_forth tree_sitter_joy)
target_include_directories(test_clap_units PRIVATE ${MIDI_LANGS_HIGHLIGHT_DIR})

# Headless VST3 host for the tests in tests/CMakeLists.txt.
if(BUILD_VST3 AND NOT WIN32)
    set(hosting ${VST3_SDK_ROOT}/public.sdk/source/vst/hosting)
    if(APPLE)
        set(module_platform ${hosting}/module_mac.mm)
        set_source_files_properties(${module_platform} PROPERTIES COMPILE_OPTIONS -fobjc-arc)
    else()
        set(module_platform ${hosting}/module_linux.cpp)
    endif()
    add_executable(test_vst3_host test_vst3_host.cpp ${hosting}/module.cpp ${module_platform}
        ${hosting}/hostclasses.cpp ${hosting}/eventlist.cpp ${hosting}/pluginterfacesupport.cpp)
    target_link_libraries(test_vst3_host PRIVATE base-sdk-vst3 ${CMAKE_DL_LIBS})
    if(APPLE)
        target_link_libraries(test_vst3_host PRIVATE "-framework Foundation" "-framework CoreFoundation")
    endif()
endif()
