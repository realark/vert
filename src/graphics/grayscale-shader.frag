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
  // weigh the values to make grayscale look accurate to the human eye
  float average = 0.2126 * texPixels.r + 0.7152 * texPixels.g + 0.0722 * texPixels.b;
  FragColor = vec4(average, average, average, 1.0);
}
