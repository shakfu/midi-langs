# CLAP plugin framework. Each language adds its plugin with midi_langs_add_clap.
set(MIDI_LANGS_CLAP_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")

add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/pugl ${CMAKE_BINARY_DIR}/thirdparty/pugl)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/imgui ${CMAKE_BINARY_DIR}/thirdparty/imgui)

# The parts that do not depend on the language or on pugl.
add_library(midi_langs_clap STATIC timeline.cpp editor_logic.cpp udp_server.cpp)
target_include_directories(midi_langs_clap PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/clap/include
)
target_compile_features(midi_langs_clap PUBLIC cxx_std_17)
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
#                     SOURCES <adapter sources> LIBRARIES <interpreter libraries>)
# Builds <build>/clap/<NAME>.clap and, with BUILD_VST3, <build>/vst3/<NAME>.vst3 (target
# <target>_vst3). The adapter defines kLanguage (language.hpp).
function(midi_langs_add_clap target)
    cmake_parse_arguments(ARG "" "NAME;CLASS_PREFIX" "SOURCES;LIBRARIES" ${ARGN})

    # The plugin code, compiled once for both formats. Objects, not an archive: the linker
    # would drop plugin.cpp, which nothing references but the exported clap_entry.
    add_library(${target}_impl OBJECT
        ${MIDI_LANGS_CLAP_DIR}/plugin.cpp
        ${MIDI_LANGS_CLAP_DIR}/editor_imgui.cpp
        ${ARG_SOURCES}
    )
    target_link_libraries(${target}_impl PUBLIC midi_langs_clap imgui_gl3 ${ARG_LIBRARIES})
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
target_link_libraries(test_clap_units PRIVATE midi_langs_clap)

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
