#version 330

in vec2 position;
in vec3 colour;
in vec2 texcoord;

out vec4 f_colour;
out vec2 f_texcoord;

void main()
{
    gl_Position = vec4(position, 0.0, 1.0);
    f_colour = vec4(colour, 1.0);
    f_texcoord = texcoord;
}
