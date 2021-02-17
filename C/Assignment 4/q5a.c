#include "image-io.h"
#include "image.h"
#include "stack.h"
#include <stdio.h>

struct image *image_load_from_input(void){
  int n = 0;
  scanf("%d", &n);
  int width = n;
  scanf("%d", &n);
  int height = n;
  n = 0;
  struct image *img = image_create(width, height, 50);
  for (int h = 0; h < height; h++) {
    for (int w = 0; w < width; w++){
      image_set_pixel(img, w, h, n);
      n++;
    }
  }
  return img;
}

void image_print(const struct image *img) {
  int width = image_get_width(img);
  int height = image_get_height(img);
  printf("%d %d\n", width, height);
  for (int h = 0; h < height; h++) {
    for (int w = 0; w < width - 1; w++) {
      printf("%d ", image_get_pixel(img, w, h));
    }
    printf("%d\n", image_get_pixel(img, width - 1, h));
  }
}