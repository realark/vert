#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
  vec4 spriteColorMod;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;

void main()
{
  vec4 texColor = texture(ourTexture, fragmentData.textureCoords);
  FragColor = fragmentData.spriteColorMod * texColor;
}
