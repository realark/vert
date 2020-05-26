#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;

void main()
{
  FragColor = vec4(vec3(1.0 - texture(ourTexture, fragmentData.textureCoords)), 1.0);
  // weigh the values to make grayscale look accurate to the human eye
  float average = 0.2126 * FragColor.r + 0.7152 * FragColor.g + 0.0722 * FragColor.b;
  FragColor = vec4(average, average, average, 1.0);
}
