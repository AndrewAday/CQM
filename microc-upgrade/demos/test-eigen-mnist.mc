extern int load_mnist_data(
  fmatrix[] dst_fm_images,
  fmatrix[] dst_fm_labels,
  string image_filename,
  string label_filename
);

int main()
{
  int ret;
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

  print_mnist_image(dst_fm_images[49999]);
  print_mat(dst_fm_labels[49999]);

  return 0;
}
