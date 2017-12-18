extern int load_mnist_data(
  fmatrix[] dst_fm_images,
  fmatrix[] dst_fm_labels,
  string image_filename,
  string label_filename
);

struct fc_model {
  fmatrix[] train_x;
  fmatrix[] train_y;
  fmatrix[] test_x;
  fmatrix[] test_y;
  fmatrix[] biases;
  fmatrix[] weights;
  int[] layer_sizes;
  int epochs;
  int mini_batch_size;
  float learning_rate;
  fp (float) weight_init;
  fp (float, float) activate;
  fp (float, float) activate_prime;
  fp (fmatrix, fmatrix, float) cost;
  fp (fmatrix, fmatrix, fmatrix) cost_prime;
}

struct backprop_deltas {
  fmatrix[] weight_deltas;
  fmatrix[] bias_deltas;
}

[struct backprop_deltas bpds] free() void {
  free_fmat_arr(bpds.weight_deltas);
  free_fmat_arr(bpds.bias_deltas);
  free(bpds);
}

[struct fc_model fc] free_model() void {
  free_fmat_arr(fc.train_x);
  free_fmat_arr(fc.train_y);
  free_fmat_arr(fc.test_x);
  free_fmat_arr(fc.test_y);
  free_fmat_arr(fc.biases);
  free_fmat_arr(fc.weights);
  return;
}

[struct fc_model fc] init_biases() void {
  int i;
  fc.biases = make(fmatrix, len(fc.layer_sizes) - 1);
  for (i = 1; i < len(fc.layer_sizes); i = i + 1) {
    fc.biases[i-1] = init_fmat_zero(fc.layer_sizes[i], 1);
    fc.biases[i-1] = populate_fmat(fc.biases[i-1], fc.weight_init);
  }
  return;
}

/* construct weight matrices and initialize with random values */
[struct fc_model fc] init_weights() void {
  int i;
  fc.weights = make(fmatrix, len(fc.layer_sizes) - 1);
  for (i = 1; i < len(fc.layer_sizes); i = i + 1) {
    fc.weights[i-1] = init_fmat_zero(fc.layer_sizes[i], fc.layer_sizes[i-1]);
    fc.weights[i-1] = populate_fmat(fc.weights[i-1], fc.weight_init);
  }
  return;
}

[struct fc_model fc] predict(fmatrix X) fmatrix {
  int i;
  fmatrix: fm, tmp1, tmp2;
  for (i = 0; i < len(fc.weights); i = i + 1) {
    tmp1 = fc.weights[i] .. X;
    tmp2 = tmp1 + fc.biases[i];
    X = tmp2 => f_fmat(fc.activate);
    free(tmp1); free(tmp2);
  }
  return X;
}

/* Zeros the update matrices for next minibatch */
[struct fc_model fc] zero_deltas(
  fmatrix[] weights, fmatrix[] biases, bool should_free) void {
  int i;
  for (i = 0; i < len(fc.weights); i = i + 1) {
    if (should_free) {
      free(weights[i]);
      free(biases[i]);
    }
    weights[i] = init_fmat_zero(
      rows(fc.weights[i]), cols(fc.weights[i])
    );
    biases[i] = init_fmat_zero(
      rows(fc.biases[i]), cols(fc.biases[i])
    );
  }
}

[struct fc_model fc] train() void {
  bool should_free;
  int: mini, e, i, train_idx, train_size, l, ratio, progress;
  struct backprop_deltas bpds;
  fmatrix[]: sum_weight_deltas, sum_bias_deltas;
  fmatrix: tmp1, tmp2, tmp3, tmp4;
  int[] train_indices;

  /* initialize training indices */
  train_size = len(fc.train_x);
  train_indices = make(int, train_size);
  for (i = 0; i < train_size; i = i + 1) {
    train_indices[i] = i;
  }
  ratio = (train_size / 30);

  /* initialize parameters and update matrices */
  fc.init_weights();
  fc.init_biases();
  sum_weight_deltas = make(fmatrix, len(fc.weights));
  sum_bias_deltas = make(fmatrix, len(fc.biases));
  should_free = false;

  /* initialize delta matrices */
  bpds = make(struct backprop_deltas);
  bpds.weight_deltas = make(fmatrix, len(fc.weights));
  bpds.bias_deltas = make(fmatrix, len(fc.biases));

  printf("Performance Before Training:\n");
  for (e = 0; e < fc.epochs; e = e + 1) {
    fc.evaluate();
    printf("Training Epoch %d: [", e+1);
    train_idx = 0;
    progress = 0;
    shuffle(train_indices);

    while (train_idx < train_size) {
      fc.zero_deltas(sum_weight_deltas, sum_bias_deltas, should_free);
      /* accumulate over mini patch */

      for (
        mini = train_idx;
        mini < min_int(train_idx + fc.mini_batch_size, train_size);
        mini = mini + 1
      ) {

        i = train_indices[mini];
        fc.backprop(fc.train_x[i], fc.train_y[i], bpds, should_free);
        for (l = 0; l < len(sum_weight_deltas); l = l+1) {
          tmp1 = sum_weight_deltas[l];
          tmp2 = sum_bias_deltas[l];
          sum_weight_deltas[l] = sum_weight_deltas[l] + bpds.weight_deltas[l];
          sum_bias_deltas[l] = sum_bias_deltas[l] + bpds.bias_deltas[l];
          free(tmp1);
          free(tmp2);
        }
        should_free = true;  // everything has been initialized once, now free

      }

      /* update network weights */
      for (l = 0; l < len(sum_weight_deltas); l = l + 1) {
        /* normalize */
        tmp1 = sum_weight_deltas[l];
        tmp2 = sum_bias_deltas[l];
        sum_weight_deltas[l] =
          sum_weight_deltas[l] * (fc.learning_rate / float_of_int(fc.mini_batch_size));
        sum_bias_deltas[l] =
          sum_bias_deltas[l] * (fc.learning_rate / float_of_int(fc.mini_batch_size));
        free(tmp1);
        free(tmp2);
        tmp3 = fc.weights[l];
        tmp4 = fc.biases[l];
        fc.weights[l] = fc.weights[l] - sum_weight_deltas[l];
        fc.biases[l] = fc.biases[l] - sum_bias_deltas[l];
        free(tmp3); free(tmp4);
      }
      train_idx = train_idx + fc.mini_batch_size;
      if (train_idx > progress) {
        printf("=");
        flush();
        progress = progress + ratio;
      }
    }
    printf("]\n");
  }
  fc.evaluate();  // final performance

  return;
}

[struct fc_model fc] backprop(
  fmatrix x, fmatrix y, struct backprop_deltas bpds, bool should_free
) void {
  int: i, num_param_layers;
  fmatrix: activation, z, z_prime, delta, actv_transpose, tmp1, tmp2, tmp3, tmp4;
  fmatrix[]: activations, zs;
  fp (float, float) activate_prime;
  fp (fmatrix, fmatrix, fmatrix) cost_prime;

  num_param_layers = len(fc.weights);

  activate_prime = fc.activate_prime;
  cost_prime = fc.cost_prime;

  activations = make(fmatrix, len(fc.layer_sizes));
  activations[0] = copy(x);
  zs = make(fmatrix, num_param_layers);

  // free from last run
  for (i = 0; i < len(fc.weights); i = i + 1) {
    if (should_free) {
      free(bpds.weight_deltas[i]);
      free(bpds.bias_deltas[i]);
    }
  }

  // forward pass
  for (i = 0; i < len(fc.weights); i = i + 1) {
    // tmp1 = (fc.weights[i] .. activation);
    tmp1 = (fc.weights[i] .. activations[i]);
    z = tmp1 + fc.biases[i];
    free(tmp1);
    zs[i] = z;
    activations[i+1] = f_fmat(z, fc.activate);
  }

  // TODO: cannot distinguish calling fp from method dispatch.
  // backward pass
  tmp1 = cost_prime(activations[len(activations) - 1], y);
  tmp2 = f_fmat(zs[len(zs)-1], activate_prime);
  delta = tmp1 * tmp2;
  free(tmp1);
  free(tmp2);

  // free(activations[len(activations) - 1]);
  bpds.bias_deltas[num_param_layers - 1] = delta;
  tmp1 = ((activations[len(activations) - 2])^);
  bpds.weight_deltas[num_param_layers - 1] = delta .. tmp1;
  free(tmp1);


  for (i = 2; i < len(fc.layer_sizes); i = i + 1) {
    z = zs[len(zs)-i];
    z_prime = f_fmat(z, activate_prime);
    tmp1 = (fc.weights[num_param_layers - i + 1])^;
    tmp2 = tmp1 .. bpds.bias_deltas[num_param_layers - i + 1];
    bpds.bias_deltas[num_param_layers - i] = tmp2 * z_prime;
    free(z_prime); free(tmp1); free(tmp2);
    // ------------The Leak----------
    tmp1 = (activations[len(activations) - i - 1]^);
    bpds.weight_deltas[num_param_layers - i] =
      bpds.bias_deltas[num_param_layers - i] .. tmp1;
    free(tmp1);
    // ------------------------------
  }

  free_fmat_arr(activations);
  free_fmat_arr(zs);

  return;
}

[struct fc_model fc] evaluate() void {
  int: i, pred, correct, gold;
  fp (fmatrix, fmatrix, float) cost_func;
  float cost;
  fmatrix out;

  cost_func = fc.cost;

  cost = 0.;
  correct = 0;
  for (i = 0; i < len(fc.test_x); i = i + 1) {
    out = fc.predict(fc.test_x[i]);
    pred = argmax(out);
    gold = argmax(fc.test_y[i]);
    cost = cost + cost_func(out, fc.test_y[i]);
    free(out);

    if (pred == gold) {
      correct = correct + 1;
    }
  }
  printf("\ttest set cost: %f\n", cost);
  printf("\ttest set accuracy: %d/%d = %f\n", correct, len(fc.test_x), float_of_int(correct) / float_of_int(len(fc.test_x)));
}

[struct fc_model fc] demo(int num) void {
  int: i, closest_guess, gold, pred;
  float: min_cost, cost;
  fmatrix out;
  fp (fmatrix, fmatrix, float) cost_func;
  int[]: correct_indices, incorrect_indices;

  cost_func = fc.cost;
  min_cost = -1.;
  cost = 0.;

  correct_indices = make(int, 0);
  incorrect_indices = make(int, 0);

  for (i = 0; i < len(fc.test_x); i = i + 1) {
    out = fc.predict(fc.test_x[i]);
    pred = argmax(out);
    gold = argmax(fc.test_y[i]);

    cost = cost_func(out, fc.test_y[i]);
    free(out);

    if (pred == gold) {
      correct_indices = append(correct_indices, i);
    } else {
      if (cost < min_cost || min_cost < 0.) {
        min_cost = cost;
        closest_guess = i;
      } else {
        incorrect_indices = append(incorrect_indices, i);
      }
    }
  }

  printf("================Correct Guesses===============\n");
  for (i = 0; i < min_int(len(correct_indices), num); i = i + 1) {
      print_mnist_image(fc.test_x[correct_indices[i]]);
      printf("Prediction: %d\n", argmax(fc.test_y[correct_indices[i]]));
  }

  printf("\n================Incorrect Guesses===============\n");
  incorrect_indices[0] = closest_guess;
  for (i = 0; i < min_int(len(incorrect_indices), num); i = i + 1) {
      print_mnist_image(fc.test_x[incorrect_indices[i]]);
      out = fc.predict(fc.test_x[incorrect_indices[i]]);
      pred = argmax(out);
      free(out);
      printf("Prediction: %d\n", pred);
  }

  return;
}

void print_mnist_image(fmatrix fm) {
  int: i, j, idx;
  float val;
  for (i = 0; i < 28; i = i + 1) {
    for (j = 0; j < 28; j = j + 1) {
      idx = 28*i + j;
      val = fm[idx, 0];
      if (val > 0.50) {
        printf("[0]");
      } else {
        if (val > 0.01 ) {
          printf("[|]");
        } else {
          printf("[ ]");
        }
      }
      if (j == 27) {
        print_line();
      }
    }
  }
}

/* Deep-learning related math functions */
int argmax(fmatrix fm) {
  int r;
  int m;
  m = 0;
  for (r = 0; r < rows(fm); r = r + 1) {
    if (fm[r,0] > fm[m,0]) {
      m = r;
    }
  }
  return m;
}

float l2_norm(fmatrix fm) {
  int r;
  float acc;
  for (r = 0; r < rows(fm); r = r + 1) {
    acc = acc + square(fm[r,0]);
  }
  return sqrt(acc);
}

float quadratic_cost(fmatrix x, fmatrix y) {
  float ret;
  fmatrix fm;
  fm = x - y;
  ret = square(l2_norm(fm)) * .5;
  free(fm);
  return ret;
}

fmatrix quadratic_cost_prime(fmatrix x, fmatrix y) {
  return (x - y);
}

float sigmoid(float z) {
    return 1.0 / (1.0 + exp(-z));
}

float sigmoid_prime(float z) {
  return sigmoid(z) * (1. - sigmoid(z));
}

float norm_init() {
  return rand_norm(0., 1.);
}
