# pugl with the OpenGL and stub backends, static. See VERSION.txt.
set(PUGL_SOURCES src/common.c src/internal.c)
if(APPLE)
	list(APPEND PUGL_SOURCES src/mac.m src/mac_gl.m src/mac_stub.m)
elseif(WIN32)
	list(APPEND PUGL_SOURCES src/win.c src/win_gl.c src/win_stub.c)
else()
	list(APPEND PUGL_SOURCES src/x11.c src/x11_gl.c src/x11_stub.c)
endif()
# Absolute, for midi_langs_add_pugl calls from other directories.
list(TRANSFORM PUGL_SOURCES PREPEND ${CMAKE_CURRENT_SOURCE_DIR}/)
set(PUGL_SOURCES ${PUGL_SOURCES} CACHE INTERNAL "")
set(PUGL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include CACHE INTERNAL "")

# One library per binary that loads into a shared process: Objective-C classes share one
# namespace, so pugl's classes get a per-binary prefix (pugl has no option for this).
function(midi_langs_add_pugl target class_prefix)
	add_library(${target} STATIC ${PUGL_SOURCES})
	target_include_directories(${target} PUBLIC ${PUGL_INCLUDE_DIR})
	target_compile_definitions(${target} PUBLIC PUGL_STATIC PRIVATE PUGL_INTERNAL)
	set_target_properties(${target} PROPERTIES C_STANDARD 99 C_VISIBILITY_PRESET hidden)
	if(APPLE)
		foreach(cls PuglWrapperView PuglWindow PuglWindowDelegate PuglOpenGLView PuglStubView)
			target_compile_definitions(${target} PRIVATE ${cls}=${class_prefix}${cls})
		endforeach()
		target_compile_definitions(${target} PRIVATE GL_SILENCE_DEPRECATION)
		target_link_libraries(${target} PUBLIC "-framework Cocoa" "-framework OpenGL" "-framework CoreVideo")
	elseif(WIN32)
		target_link_libraries(${target} PUBLIC opengl32 dwmapi gdi32 shell32 shlwapi user32)
	else()
		find_package(X11 REQUIRED)
		find_package(OpenGL REQUIRED COMPONENTS OpenGL GLX)
		# Optional X extensions off: fewer system dependencies, at the cost of cursor themes and refresh-rate sync.
		target_compile_definitions(${target} PRIVATE USE_XCURSOR=0 USE_XRANDR=0 USE_XSYNC=0)
		target_link_libraries(${target} PUBLIC X11::X11 OpenGL::GL OpenGL::GLX)
	endif()
endfunction()

# Each plugin calls midi_langs_add_pugl with its own prefix (projects/clap/CMakeLists.txt).
