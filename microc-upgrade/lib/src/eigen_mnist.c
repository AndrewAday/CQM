#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "eigen_test.h"

/* code copied and modified from https://github.com/projectgalateia/mnist/ */
/* to build: make eigen_mnist */

static unsigned int mnist_bin_to_int(char *v)
{
	int i;
	unsigned int ret = 0;

	for (i = 0; i < 4; ++i) {
		ret <<= 8;
		ret |= (unsigned char)v[i];
	}

	return ret;
}

int load_mnist_data(
  matrix_t *dst_fm_images,
  matrix_t *dst_fm_labels,
  char *image_filename,
  char *label_filename
) {
  int return_code = 0;
  int i;
  char tmp[4];
  matrix_t *src_fm_images;
  matrix_t *src_fm_labels;

  unsigned int image_cnt, label_cnt;
  unsigned int image_dim[2];

  FILE *ifp = fopen(image_filename, "rb");
  FILE *lfp = fopen(label_filename, "rb");

  if (!ifp || !lfp) {
    return_code = -1; /* No such files */
    goto cleanup;
  }

  fread(tmp, 1, 4, ifp);
  if (mnist_bin_to_int(tmp) != 2051) {
    return_code = -2; /* Not a valid image file */
    goto cleanup;
  }

  fread(tmp, 1, 4, lfp);
  if (mnist_bin_to_int(tmp) != 2049) {
    return_code = -3; /* Not a valid label file */
    goto cleanup;
  }

  fread(tmp, 1, 4, ifp);
  image_cnt = mnist_bin_to_int(tmp);

  fread(tmp, 1, 4, lfp);
  label_cnt = mnist_bin_to_int(tmp);

  if (image_cnt != label_cnt) {
    return_code = -4; /* Element counts of 2 files mismatch */
    goto cleanup;
  }

  for (i = 0; i < 2; ++i) {
    fread(tmp, 1, 4, ifp);
    image_dim[i] = mnist_bin_to_int(tmp);
  }

  if (image_dim[0] != 28 || image_dim[1] != 28) {
    return_code = -2; /* Not a valid image file */
    goto cleanup;
  }

  src_fm_images = (matrix_t *)malloc(sizeof(matrix_t) * image_cnt);
  src_fm_labels = (matrix_t *)malloc(sizeof(matrix_t) * label_cnt);

  for (i = 0; i < image_cnt; ++i) {
    if (i % 1000 == 0)
      printf("reading image %d/%d\n", i, image_cnt);

    int j;
    unsigned char read_data[28 * 28];

    src_fm_images[i] = init_fmat_zero(28*28, 1);
    fread(read_data, 1, 28*28, ifp);
    for (j = 0; j < 28*28; ++j) {
      mat_index_assign(src_fm_images[i], j, 0, read_data[j] / 255.0);
    }

    src_fm_labels[i] = init_fmat_zero(10, 1);
    fread(tmp, 1, 1, lfp);
    mat_index_assign(src_fm_labels[i], tmp[0], 0, 1.0);
    // printf("label: %d\n", tmp[0]);
  }

  memcpy(dst_fm_images, src_fm_images, sizeof(matrix_t) * image_cnt);
  memcpy(dst_fm_labels, src_fm_labels, sizeof(matrix_t) * label_cnt);

  cleanup:
  if (ifp) fclose(ifp);
  if (lfp) fclose(lfp);

  return return_code;
}


/* DO NOT DELETE! this is effecitvely a unit test for load_mnist_data */
/*
int main()
{
  matrix_t *dst_fm_images;
  matrix_t *dst_fm_labels;
  int ret;

  dst_fm_images = (matrix_t *)malloc(sizeof(matrix_t) * 50000);
  dst_fm_labels = (matrix_t *)malloc(sizeof(matrix_t) * 50000);

  // ret = load_mnist_data(dst_fm_images, dst_fm_labels,
  //   "mnist_data/train-images-idx3-ubyte",
  //   "mnist_data/train-labels-idx1-ubyte"
  // );

  ret = load_mnist_data(dst_fm_images, dst_fm_labels,
    "mnist_data/t10k-images-idx3-ubyte",
    "mnist_data/t10k-labels-idx1-ubyte"
  );

  printf("returned with value: %d\n", ret);

  int idx;
  double val;
  for (int i = 0; i < 28; ++i) {
    for (int j = 0; j < 28; ++j) {
      idx = 28*i + j;
      val = mat_index(dst_fm_images[9000], idx, 0);
      if (val > 0.01)
        printf("[0]");
      else
        printf("[ ]");
      if (j == 27)
        printf("\n");
    }
  }

  // print_mat(dst_fm_images[0]);
  print_mat(dst_fm_labels[9000]);

  return 0;
}
*/
