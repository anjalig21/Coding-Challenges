// This is a module for image dithering

#include "image.h"

// image_dither(img) converts img from a greyscale image to a pure
//   black and white image using Floyd-Steinberg dithering
// effects: modifies img
void image_dither(struct image *img);