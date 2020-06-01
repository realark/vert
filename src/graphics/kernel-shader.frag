#version 330 core
out vec4 FragColor;
in VertexData {
  vec2 textureCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;
uniform float kernel[9];

const float offset = 1.0 / 300.0;

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
    vec2 offsets[9] = vec2[](
        vec2(-offset,  offset), // top-left
        vec2( 0.0f,    offset), // top-center
        vec2( offset,  offset), // top-right
        vec2(-offset,  0.0f),   // center-left
        vec2( 0.0f,    0.0f),   // center-center
        vec2( offset,  0.0f),   // center-right
        vec2(-offset, -offset), // bottom-left
        vec2( 0.0f,   -offset), // bottom-center
        vec2( offset, -offset)  // bottom-right
    );

    vec4 sampleTex[9];
    for(int i = 0; i < 9; i++)
      {
        sampleTex[i] = texture(ourTexture, vec2(fragmentData.textureCoords.x,
                                                vertYCoordToTextureCoord(fragmentData.textureCoords.y + offsets[i].y)));
      }
    vec4 col = vec4(0.0);
    for(int i = 0; i < 9; i++)
      {
        col += sampleTex[i] * kernel[i];
      }

    FragColor = col;
}
