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

void main()
{
  // solid color
  // FragColor = vec4(0.0, 1.0, 1.0, 1.0);
  // color mod
  // FragColor = texture(ourTexture, fragmentData.textureCoords) * vec4(0.0, 1.0, 1.0, 1.0) ;
  // render texture if bound
  if (colorBlendFn == BLEND_MULT)
    {
      FragColor = vec4(vec3(texture(ourTexture, fragmentData.textureCoords)), 1.0) * colorMod;
    }
  else
    {
      FragColor = vec4(vec3(texture(ourTexture, fragmentData.textureCoords)), 1.0) + colorMod;
    }
  // FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
