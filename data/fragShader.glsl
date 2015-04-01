#version 330

in vec4 f_colour;
in vec2 f_texcoord;

out vec4 outColor;

uniform sampler2D tex;

void main()
{
  outColor = f_colour * texture(tex, f_texcoord);
}
