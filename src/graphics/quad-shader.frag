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

void main()
{
  vec4 texPixels = texture(ourTexture, vec2(fragmentData.textureCoords.x, fragmentData.textureCoords.y));

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
