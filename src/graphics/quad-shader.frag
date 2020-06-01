#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;
uniform vec4 colorMod;
uniform int colorBlendFn;
float BLEND_MULT = 0;
float BLEND_ADD = 1;

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
  vec4 texPixels = texture(ourTexture, vec2(fragmentData.textureCoords.x, vertYCoordToTextureCoord(fragmentData.textureCoords.y)));

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
