#include <cairo.h>

cairo_surface_t *page_image_surface;
cairo_t *page_cr;
cairo_surface_t *view_image_surface;
cairo_t *view_cr;
cairo_matrix_t matrix;
bool viewIs

void init_(){
    page_image_surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, x, y);
    page_cr = cairo_create(page_image_surface);
}

void free_(){
    cairo_destroy(image_surface_cr);
    cairo_surface_destroy(image_surface);
}

void draw_cb_scratch(cairo_t *gtk_cr){
}

void draw_cb_no_scratch(cairo_t *gtk_cr, int x, int y, int dx, int dy){
    view_image_surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, x, y);
    view_cr = cairo_create(view_image_surface);
    cairo_set_source_rgb(view_cr, 0.0, 0.0, 1.0);
    cairo_set_source_surface(view_cr, page_image_surface, dx, dy);
    cairo_paint(view_cr);
    cairo_destroy(view_cr);
}

void scale_abs(double sx, double sy){
    cairo_identity_matrix(image_surface_cr);
    cairo_scale(image_surface_cr, sx, sy);
}

void translate_rel(double tx, double ty){
    cairo_translate(image_surface_cr, tx, ty);
}
