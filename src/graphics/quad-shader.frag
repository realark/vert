#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
  vec2 vertexCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;
uniform vec4 colorMod;
uniform int colorBlendFn;
const int BLEND_MULT = 0;
const int BLEND_ADD = 1;

uniform vec4 colorMapFrom;
uniform vec4 colorMapTo;
uniform float colorMapTolerance;

void main()
{
  vec4 texPixels = texture(ourTexture, vec2(fragmentData.textureCoords.x, fragmentData.textureCoords.y));

  if (colorMapFrom != colorMapTo)
    {
      if ((abs(colorMapFrom.r - texPixels.r) <= colorMapTolerance)
          && (abs(colorMapFrom.g - texPixels.g) <= colorMapTolerance)
          && (abs(colorMapFrom.b - texPixels.b) <= colorMapTolerance)
          && (abs(colorMapFrom.a - texPixels.a) <= colorMapTolerance))
        {
          texPixels = colorMapTo * texPixels;
        }
    }

  // render texture if bound
  if (colorBlendFn == BLEND_MULT)
    {
      FragColor = texPixels * colorMod;
    }
  else
    {
      FragColor = texPixels + colorMod;
    }

  // debugging. 0,0 == black. 1,1 == yellow
  // FragColor = vec4(fragmentData.textureCoords.x, fragmentData.textureCoords.y, 0.0, 1.0);
}
