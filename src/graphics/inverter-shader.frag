#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
  vec2 vertexCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;

void main()
{
  vec4 texPixels = texture(ourTexture, vec2(fragmentData.textureCoords.x, fragmentData.textureCoords.y));
  FragColor = vec4(1.0 - texPixels.rgb, texPixels.a);
}
