int main()
{
  struct fc_model fc;
  fmatrix[]: train_fm_images, train_fm_labels, test_fm_images, test_fm_labels;
  int[] layer_sizes;
  int: epochs, mini_batch_size;
  float learning_rate;

  /* seed random number generator */
  srand(time());

  /* define hyperparameters */
  epochs = 20;
  learning_rate = 1.;
  mini_batch_size = 20;
  layer_sizes = (int[]) {784, 50, 10};

  /* allocate memory */
  fc = make(struct fc_model);

  train_fm_images = make(fmatrix, 50000);
  train_fm_labels = make(fmatrix, 50000);

  test_fm_images = make(fmatrix, 10000);
  test_fm_labels = make(fmatrix, 10000);

  /* Load train */
  load_mnist_data(train_fm_images, train_fm_labels,
    "mnist_data/train-images-idx3-ubyte",
    "mnist_data/train-labels-idx1-ubyte"
  );

  /* Load test */
  load_mnist_data(test_fm_images, test_fm_labels,
    "mnist_data/t10k-images-idx3-ubyte",
    "mnist_data/t10k-labels-idx1-ubyte"
  );

  /* Popuate fc model fields */
  fc.train_x = train_fm_images;
  fc.train_y = train_fm_labels;
  fc.test_x = test_fm_images;
  fc.test_y = test_fm_labels;
  fc.layer_sizes = layer_sizes;
  fc.epochs = epochs;
  fc.mini_batch_size = mini_batch_size;
  fc.learning_rate = learning_rate;
  fc.weight_init = norm_init;
  fc.activate = sigmoid;
  fc.activate_prime = sigmoid_prime;
  // fc.activate = tanh;
  // fc.activate_prime = tanh_prime;
  // fc.activate = relu;
  // fc.activate_prime = relu_prime;
  fc.cost = quadratic_cost;
  fc.cost_prime = quadratic_cost_prime;

  fc.train();
  fc.demo(5);

  return 0;
}
