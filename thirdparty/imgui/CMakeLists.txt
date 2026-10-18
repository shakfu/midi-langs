# Dear ImGui core with the OpenGL 3 renderer, static. See VERSION.txt.
add_library(imgui_gl3 STATIC
	imgui.cpp imgui_draw.cpp imgui_tables.cpp imgui_widgets.cpp
	backends/imgui_impl_opengl3.cpp
	misc/cpp/imgui_stdlib.cpp)
target_include_directories(imgui_gl3 PUBLIC . backends misc/cpp)
target_compile_features(imgui_gl3 PUBLIC cxx_std_17)
set_target_properties(imgui_gl3 PROPERTIES CXX_VISIBILITY_PRESET hidden)
# The OpenGL 3 loader opens the GL library at runtime.
target_link_libraries(imgui_gl3 PUBLIC ${CMAKE_DL_LIBS})
