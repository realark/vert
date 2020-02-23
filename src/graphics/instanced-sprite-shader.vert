#version 330 core
layout (location = 0) in vec3 localCoord;
layout (location = 1) in vec2 srcCoord;
layout (location = 2) in mat4 worldModel;
layout (location = 6) in vec4 spriteSrc;
layout (location = 7) in vec4 spriteColor;

out VertexData {
  vec2 textureCoords;
  vec4 spriteColorMod;
} vertexData;

uniform mat4 worldProjection;

float roundingPrecision = 10000.0;

void main()
{
  float spriteSrcX = spriteSrc.x;
  float spriteSrcY = spriteSrc.y;
  float spriteWidth = spriteSrc.z;
  float spriteHeight = spriteSrc.w;
  vertexData.textureCoords = vec2(spriteSrcX + (srcCoord.x * spriteWidth),
                                  spriteSrcY + (srcCoord.y * spriteHeight));
  vertexData.spriteColorMod = spriteColor;

  vec4 rawPosition = worldProjection * worldModel * vec4(localCoord, 1.0);
  gl_Position = vec4(round((rawPosition.x * roundingPrecision)) / roundingPrecision,
                     round((rawPosition.y * roundingPrecision)) / roundingPrecision,
                     rawPosition.z,
                     rawPosition.a);
}
