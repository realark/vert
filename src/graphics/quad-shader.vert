#version 330 core
layout (location = 0) in vec3 screenPos;
layout (location = 1) in vec2 srcCoord;

out VertexData {
  vec2 textureCoords;
} vertexData;

uniform mat4 worldModel;
uniform mat4 worldProjection;

// TODO: rename textureSrc
uniform vec4 spriteSrc;

void main()
{
  float spriteSrcX = spriteSrc.x;
  float spriteSrcY = spriteSrc.y;
  float spriteWidth = spriteSrc.z;
  float spriteHeight = spriteSrc.w;

  vertexData.textureCoords = vec2(spriteSrcX + (srcCoord.x * spriteWidth),
                                  spriteSrcY + (srcCoord.y * spriteHeight));
  gl_Position = worldProjection * worldModel * vec4(screenPos, 1.0);
}
