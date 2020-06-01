#version 330 core
layout (location = 0) in vec3 screenPos;
layout (location = 1) in vec2 srcCoord;

out VertexData {
  vec2 textureCoords;
} vertexData;

uniform mat4 worldModel;
uniform mat4 worldProjection;

// TODO: rename textureSrc
uniform vec4 textureSrc;

void main()
{
  float textureSrcX = textureSrc.x;
  float textureSrcY = textureSrc.y;
  float spriteWidth = textureSrc.z;
  float spriteHeight = textureSrc.w;

  vertexData.textureCoords = vec2(textureSrcX + (srcCoord.x * spriteWidth),
                                  (textureSrcY + (srcCoord.y * spriteHeight)));
  gl_Position = worldProjection * worldModel * vec4(screenPos.x, screenPos.y, screenPos.z, 1.0);
}
