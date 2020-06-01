#version 330 core
out vec4 FragColor;

in VertexData {
  vec2 textureCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;

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
  vec4 texPixels = vec4(vec3(texture(ourTexture, vec2(fragmentData.textureCoords.x, vertYCoordToTextureCoord(fragmentData.textureCoords.y)))), 1.0);
  // weigh the values to make grayscale look accurate to the human eye
  float average = 0.2126 * texPixels.r + 0.7152 * texPixels.g + 0.0722 * texPixels.b;
  FragColor = vec4(average, average, average, 1.0);
}
