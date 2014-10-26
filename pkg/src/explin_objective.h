/*
 *  Author: Niels Richard Hansen
 */

#ifndef EXPLIN_OBJECTIVE_H_
#define EXPLIN_OBJECTIVE_H_

template < typename T >
class expLinLoss {

public:

	const sgl::natural n_samples;
	const sgl::natural n_groups;

private:

	sgl::natural_vector const& G; // sample-parameter grouping - vector of length n_samples
  sgl::vector const& W; // weights (interdistances) between samples
	sgl::vector const& Y; // response - vector of length n_samples
  sgl::numeric c; // switch-point from exponential to linear

	sgl::matrix lp; //linear predictors - matrix of size n_samples x n_groups

	mutable sgl::matrix_field hessian_matrices;
	mutable bool hessians_computed;

public:

  typedef sgl::hessian_full<false> hessian_type;
	typedef sgl::DataPackage_4< sgl::MatrixData<T>,
  		sgl::GroupData,
			sgl::Data<sgl::vector, 'W'>,
      sgl::Data<sgl::vector, 'Y'> > data_type;

	expLinLoss()
			: 	n_samples(0),
			  	n_groups(0),
			  	G(sgl::null_natural_vector),
			  	W(sgl::null_vector),
			  	Y(sgl::null_vector),
          c(sgl::null_vector),
			  	lp(n_samples, n_groups),
			  	hessian_matrices(static_cast < sgl::natural >(0)),
			  	hessians_computed(false)
	{
	}

	expLinLoss(data_type const& data)
			: 	n_samples(data.get_A().n_samples),
				n_groups(data.get_B().n_groups),
				G(data.get_B().grouping),
				W(data.get_C().data),
				Y(data.get_D().data),
        c(static_cast < sgl::natural >(0)),
				lp(n_samples, n_groups),
				hessian_matrices(n_samples),
				hessians_computed(false)
	{
	}

	void set_lp(sgl::matrix const& lp)
	{
		this->lp = lp;

		hessians_computed = false;
	}

	void set_lp_zero()
	{
		lp.zeros(n_samples, n_groups);

		hessians_computed = false;
	}

	const sgl::matrix gradients() const
	{

    sgl::numeric eta;
		sgl::matrix grad = arma::zeros < sgl::matrix > (n_groups, n_samples);

		for (sgl::natural i = 0; i < n_samples; ++i)
		{
      eta = lp(i, G(i));
      if (eta <= c) {
        grad(G(i), i) = exp(eta) * W(i) - Y(i);
      } else {
        grad(G(i), i) = exp(c) * W(i) - Y(i) / (eta - c + 1);
      }
		}
    
		return grad;

	}

	void compute_hessians() const
	{

		if (hessians_computed)
		{
			return;
		}

    sgl::numeric eta;

		for (sgl::natural i = 0; i < n_samples; ++i)
		{
			hessian_matrices(i).zeros(n_groups, n_groups);
      eta = lp(i, G(i));
      if (eta <= c) {
  		  hessian_matrices(i)(G(i), G(i)) =  exp(eta) * W(i);
      } else {
        hessian_matrices(i)(G(i), G(i)) =  Y(i) / ((eta - c + 1) * (eta - c + 1));
      }
		}

		hessians_computed = true;
	}

	const sgl::matrix& hessians(sgl::natural i) const
	{
		return hessian_matrices(i);
	}

	const sgl::numeric sum_values() const
	{

		sgl::numeric value = 0, eta;
		for (sgl::natural i = 0; i < n_samples; ++i)
		{
      eta = lp(i, G(i));
      if (eta <= c) {
        value += exp(eta) * W(i) - Y(i) * eta;
      } else {
        value += exp(c) * (eta - c + 1) * W(i) - Y(i) * (c + log(eta - c + 1));
      }
      
		}

		return value;
	}

};

typedef sgl::ObjectiveFunctionType < sgl::GenralizedLinearLossDense < expLinLoss < sgl::matrix > > ,
		expLinLoss < sgl::matrix >::data_type > explin;

typedef sgl::ObjectiveFunctionType <
		sgl::GenralizedLinearLossSparse < expLinLoss < sgl::sparse_matrix > > ,
		expLinLoss < sgl::sparse_matrix >::data_type > explin_spx;

#endif /* EXPLIN_OBJECTIVE_H_ */
