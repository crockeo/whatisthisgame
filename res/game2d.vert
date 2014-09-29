#version 150
uniform mat3 cam;
in vec2 vertexCoord;
in vec2 textureCoord;
out vec2 texCoordFrag;

void main() {
  texCoordFrag = textureCoord;
  gl_Position = vec4(cam * (vec3(vertexCoord, 1) * 2 - 1), 1);
}
