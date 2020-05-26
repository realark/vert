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
}
