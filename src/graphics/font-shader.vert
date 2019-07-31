#version 330 core
layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
out vec2 TexCoords;

uniform mat4 projection;
float roundingPrecision = 10000.0;

void main()
{

  vec4 rawPosition = projection * vec4(vertex.xy, 0.0, 1.0);
  gl_Position = vec4(round((rawPosition.x * roundingPrecision)) / roundingPrecision,
                     round((rawPosition.y * roundingPrecision)) / roundingPrecision,
                     rawPosition.z,
                     rawPosition.a);
  TexCoords = vertex.zw;
}
