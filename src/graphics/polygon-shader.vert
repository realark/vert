#version 330 core
layout (location = 0) in vec3 screenPos;

uniform mat4 worldModel;
uniform mat4 worldProjection;

void main()
{
  gl_Position = worldProjection * worldModel * vec4(screenPos, 1.0);
}
