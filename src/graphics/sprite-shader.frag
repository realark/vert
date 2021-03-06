#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;
uniform vec4 spriteColorMod;
uniform vec4 spriteColorMapFrom;
uniform vec4 spriteColorMapTo;
uniform float spriteColorMapTolerance;

void main()
{
  vec4 colorMappedTexture = texture(ourTexture, fragmentData.textureCoords);
  if (spriteColorMapFrom != spriteColorMapTo) {
    if ((abs(spriteColorMapFrom.r - colorMappedTexture.r) <= spriteColorMapTolerance)
        && (abs(spriteColorMapFrom.g - colorMappedTexture.g) <= spriteColorMapTolerance)
        && (abs(spriteColorMapFrom.b - colorMappedTexture.b) <= spriteColorMapTolerance)
        && (abs(spriteColorMapFrom.a - colorMappedTexture.a) <= spriteColorMapTolerance)) {
      colorMappedTexture = spriteColorMapTo * colorMappedTexture;
    }
  }
  FragColor = spriteColorMod * colorMappedTexture;
}
