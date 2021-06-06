#include <cairo.h>

int main (int argc, char *argv[])
{
    cairo_surface_t *surface =
        cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 500, 500);
    cairo_surface_t *surface_output =
        cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 100, 100);
    cairo_t *cr =
        cairo_create (surface);
    cairo_t *cr_out =
        cairo_create (surface_output);

    cairo_select_font_face (cr, "serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, 32.0);
    cairo_set_source_rgb (cr, 0.0, 0.0, 1.0);
    cairo_translate(cr, 10, 0);
    cairo_move_to (cr, 10.0, 50.0);
    cairo_matrix_t matrix;
    cairo_matrix_init_identity(&matrix);
    cairo_matrix_translate(&matrix, 10, 0);
    cairo_scale(cr, 30, 30);
    cairo_rotate(cr, 1);
    cairo_show_text (cr, "Hello, world");

    cairo_set_source_surface(cr_out, surface, -200, -200);
    cairo_rectangle(cr_out, 50, 50, 100, 100);
    cairo_fill(cr_out);

    cairo_destroy (cr);
    cairo_destroy (cr_out);
    cairo_surface_write_to_png (surface_output, "hello.png");
    cairo_surface_destroy (surface);
    cairo_surface_destroy (surface_output);
    return 0;
}
