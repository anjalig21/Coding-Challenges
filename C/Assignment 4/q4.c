#include <stdio.h>
#include <assert.h>
#include "image-dither.h"
#include "image-io.h"
#include "round.h"

int error_calc(int num, int error) {
  return div_round(num * error, 16);
}

void image_dither(struct image *img) {
  assert(img);
  const int ROUNDING = 127;
  const int width = image_get_width(img);
  const int height = image_get_height(img);
  int error = 0;
  int grey_colour = 0;
  for (int h = 0; h < height; h++) {
    for (int w = 0; w < width; w++) {
      grey_colour = image_get_pixel(img, w, h);
      if (grey_colour > ROUNDING) {
        image_set_pixel(img, w, h, 255);
        error = grey_colour - 255;
      } 
      else {
        image_set_pixel(img, w, h, 0);
        error = grey_colour;
      }
      if ((w + 1 < width) && (h + 1 < height)) {
        int bottom_right = image_get_pixel(img, w + 1, h + 1)
          + error_calc(1, error);
        if (bottom_right < 0) {
          bottom_right = 0;
        } 
        else if (bottom_right > 255) {
          bottom_right = 255;
        }
        image_set_pixel(img, w + 1, h + 1, bottom_right);
      }
      if (h + 1 < height) {
        int bottom_mid = image_get_pixel(img, w, h + 1) + 
          error_calc(5, error);
        if (bottom_mid < 0) {
          bottom_mid = 0;
        } 
        else if (bottom_mid > 255) {
          bottom_mid = 255; 
        } 
        image_set_pixel(img, w, h + 1, bottom_mid);
      }
      if ((w - 1 >= 0) && (h + 1 < height)) {
        int bottom_left = image_get_pixel(img, w - 1, h + 1) + 
          error_calc(3, error);
        if (bottom_left < 0) {
          bottom_left = 0;
        } 
        else if (bottom_left > 255) {
          bottom_left = 255;
        }
        image_set_pixel(img, w - 1, h + 1, bottom_left);
      }
      if (w + 1 < width) {
        int right = image_get_pixel(img, w + 1, h) + 
          error_calc(7, error);
        if (right < 0) {
          right = 0;
        } 
        else if (right > 255) {
          right = 255;
        }
        image_set_pixel(img, w + 1, h, right);
      }
    }
  }
}