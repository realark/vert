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
  FragColor = vec4(1.0 - vec3(texture(ourTexture, vec2(fragmentData.textureCoords.x, vertYCoordToTextureCoord(fragmentData.textureCoords.y)))), 1.0);
}
