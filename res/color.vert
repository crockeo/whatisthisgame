#version 150
uniform mat3 cam;
in vec2 vertexCoord;
in vec4 color;
out vec4 colorFrag;

void main() {
	colorFrag = color;
	gl_Position = vec4(cam * (vec3(vertexCoord, 1) * 2 - 1), 1);
}
