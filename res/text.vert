#version 150

uniform mat3 cam;
in vec2 vertexCoord;

void main() {
    vec4 temp = vec4(cam * vec3(vertexCoord, 1) * 2 - 1, 1);
    gl_Position = (temp / vec4(14, 14, 14, 1));
}