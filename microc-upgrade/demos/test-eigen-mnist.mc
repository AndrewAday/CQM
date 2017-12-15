extern int load_mnist_data(
  fmatrix[] dst_fm_images,
  fmatrix[] dst_fm_labels,
  string image_filename,
  string label_filename
);

int main()
{
  int ret;
  int idx;
  int i;
  int j;
  float val;
  fmatrix[] dst_fm_images;
  fmatrix[] dst_fm_labels;
  fmatrix fm;

  dst_fm_images = make(fmatrix, 50000);
  dst_fm_labels = make(fmatrix, 50000);

  ret = load_mnist_data(dst_fm_images, dst_fm_labels,
    "mnist_data/train-images-idx3-ubyte",
    "mnist_data/train-labels-idx1-ubyte"
  );

  // ret = load_mnist_data(dst_fm_images, dst_fm_labels,
  //   "mnist_data/t10k-images-idx3-ubyte",
  //   "mnist_data/t10k-labels-idx1-ubyte"
  // );

  printf("returned with value: %d\n", ret);


  for (i = 0; i < 28; i = i + 1) {
    for (j = 0; j < 28; j = j + 1) {
      idx = 28*i + j;
      fm = dst_fm_images[6794];
      val = fm[idx, 0];
      if (val > 0.01) {
        printf("[0]");
      } else {
        printf("[ ]");
      }
      if (j == 27) {
        printf("\n");
      }
    }
  }

  print_mat(dst_fm_labels[6794]);

  return 0;
}
