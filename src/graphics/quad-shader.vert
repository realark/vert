#version 330 core
layout (location = 0) in vec3 screenPos;
layout (location = 1) in vec2 srcCoord;

out VertexData {
  vec2 textureCoords;
  vec2 vertexCoords;
} vertexData;

uniform mat4 worldModel;
uniform mat4 worldProjection;

uniform vec4 textureSrc;

/**
 *  Vert renders with 0,0 == upper-left and 1,1 == lower-right.
 *  Glsl considers 1 to be "Up".
 *  This fn converts Y coord from vert to glsl so texture selection works.
 */
float vertYCoordToTextureCoord (float y)
{
  return 1.0 - y;
}

void main()
{
  float textureSrcX = textureSrc.x;
  float textureSrcY = textureSrc.y;
  float spriteWidth = textureSrc.z;
  float spriteHeight = textureSrc.w;

  vertexData.textureCoords = vec2(textureSrcX + (srcCoord.x * spriteWidth),
                                  vertYCoordToTextureCoord(textureSrcY + (srcCoord.y * spriteHeight)));
  vertexData.vertexCoords = screenPos.xy;
  gl_Position = worldProjection * worldModel * vec4(screenPos.x, screenPos.y, screenPos.z, 1.0);
}
