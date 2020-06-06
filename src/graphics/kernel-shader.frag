#version 330 core
out vec4 FragColor;
in VertexData {
  vec2 textureCoords;
  vec2 vertexCoords;
} fragmentData;

// texture sampler
uniform sampler2D ourTexture;
uniform float kernel[9];

const float offset = 1.0 / 300.0;

uniform vec4 colorMod;
uniform int colorBlendFn;
const int BLEND_MULT = 0;
const int BLEND_ADD = 1;

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
    vec3 sampleTex[9];
    float alpha[9];
    for(int i = 0; i < 9; i++)
      {
        vec4 s = vec4(texture(ourTexture, fragmentData.textureCoords + offsets[i]));
        sampleTex[i] = s.rgb;
        alpha[i] = s.a;
      }
    vec4 col = vec4(0.0, 0.0, 0.0, 1.0);
    for(int i = 0; i < 9; i++)
      {
        col.rgb += sampleTex[i] * kernel[i];
        col.a *= alpha[i];
      }

    FragColor = col;
    if (colorBlendFn == BLEND_MULT)
      {
        FragColor *= colorMod;
      }
    else
      {
        FragColor += colorMod;
      }
}
