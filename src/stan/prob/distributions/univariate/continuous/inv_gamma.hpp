#ifndef STAN__PROB__DISTRIBUTIONS__UNIVARIATE__CONTINUOUS__INV_GAMMA_HPP
#define STAN__PROB__DISTRIBUTIONS__UNIVARIATE__CONTINUOUS__INV_GAMMA_HPP

#include <boost/random/gamma_distribution.hpp>
#include <boost/random/variate_generator.hpp>
#include <stan/agrad/partials_vari.hpp>
#include <stan/error_handling/scalar/check_consistent_sizes.hpp>
#include <stan/error_handling/scalar/check_greater_or_equal.hpp>
#include <stan/error_handling/scalar/check_less_or_equal.hpp>
#include <stan/error_handling/scalar/check_nonnegative.hpp>
#include <stan/error_handling/scalar/check_not_nan.hpp>
#include <stan/error_handling/scalar/check_positive_finite.hpp>
#include <stan/math/functions/constants.hpp>
#include <stan/math/functions/value_of.hpp>
#include <stan/math/functions/lgamma.hpp>
#include <stan/math/functions/gamma_q.hpp>
#include <stan/math/functions/digamma.hpp>
#include <stan/meta/traits.hpp>
#include <stan/prob/constants.hpp>
#include <stan/prob/internal_math/math/grad_reg_inc_gamma.hpp>
#include <stan/prob/traits.hpp>

namespace stan {

  namespace prob {

    /**
     * The log of an inverse gamma density for y with the specified
     * shape and scale parameters.
     * Shape and scale parameters must be greater than 0.
     * y must be greater than 0.
     * 
     * @param y A scalar variable.
     * @param alpha Shape parameter.
     * @param beta Scale parameter.
     * @throw std::domain_error if alpha is not greater than 0.
     * @throw std::domain_error if beta is not greater than 0.
     * @throw std::domain_error if y is not greater than 0.
     * @tparam T_y Type of scalar.
     * @tparam T_shape Type of shape.
     * @tparam T_scale Type of scale.
     */
    template <bool propto,
              typename T_y, typename T_shape, typename T_scale>
    typename return_type<T_y,T_shape,T_scale>::type
    inv_gamma_log(const T_y& y, const T_shape& alpha, const T_scale& beta) {
      static const std::string function("stan::prob::inv_gamma_log");
      typedef typename stan::partials_return_type<T_y,T_shape,T_scale>::type 
        T_partials_return;

      using stan::is_constant_struct;
      using stan::error_handling::check_not_nan;
      using stan::error_handling::check_positive_finite;
      using boost::math::tools::promote_args;
      using stan::error_handling::check_consistent_sizes;
      using stan::math::value_of;

      // check if any vectors are zero length
      if (!(stan::length(y) 
            && stan::length(alpha) 
            && stan::length(beta)))
        return 0.0;

      // set up return value accumulator
      T_partials_return logp(0.0);

      check_not_nan(function, "Random variable", y);
      check_positive_finite(function, "Shape parameter", alpha);
      check_positive_finite(function, "Scale parameter", beta);
      check_consistent_sizes(function,
                             "Random variable", y,
                             "Shape parameter", alpha,
                             "Scale parameter", beta);

      // check if no variables are involved and prop-to
      if (!include_summand<propto,T_y,T_shape,T_scale>::value)
        return 0.0;

      // set up template expressions wrapping scalars into vector views
      VectorView<const T_y> y_vec(y);
      VectorView<const T_shape> alpha_vec(alpha);
      VectorView<const T_scale> beta_vec(beta);

      for (size_t n = 0; n < length(y); n++) {
        const T_partials_return y_dbl = value_of(y_vec[n]);
        if (y_dbl <= 0)
          return LOG_ZERO;
      }

      size_t N = max_size(y, alpha, beta);
      agrad::OperandsAndPartials<T_y, T_shape, T_scale> 
        operands_and_partials(y, alpha, beta);
      
      using stan::math::lgamma;
      using stan::math::digamma;

      VectorBuilder<include_summand<propto,T_y,T_shape>::value,
                    T_partials_return, T_y> log_y(length(y));
      VectorBuilder<include_summand<propto,T_y,T_scale>::value,
                    T_partials_return, T_y> inv_y(length(y));
      for(size_t n = 0; n < length(y); n++) {
        if (include_summand<propto,T_y,T_shape>::value)
          if (value_of(y_vec[n]) > 0)
            log_y[n] = log(value_of(y_vec[n]));
        if (include_summand<propto,T_y,T_scale>::value)
          inv_y[n] = 1.0 / value_of(y_vec[n]);
      }

      VectorBuilder<include_summand<propto,T_shape>::value,
                    T_partials_return, T_shape> lgamma_alpha(length(alpha));
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> digamma_alpha(length(alpha));
      for (size_t n = 0; n < length(alpha); n++) {
        if (include_summand<propto,T_shape>::value)
          lgamma_alpha[n] = lgamma(value_of(alpha_vec[n]));
        if (!is_constant_struct<T_shape>::value)
          digamma_alpha[n] = digamma(value_of(alpha_vec[n]));
      }

      VectorBuilder<include_summand<propto,T_shape,T_scale>::value,
                    T_partials_return, T_scale> log_beta(length(beta));
      if (include_summand<propto,T_shape,T_scale>::value)
        for (size_t n = 0; n < length(beta); n++)
          log_beta[n] = log(value_of(beta_vec[n]));

      for (size_t n = 0; n < N; n++) {
        // pull out values of arguments
        const T_partials_return alpha_dbl = value_of(alpha_vec[n]);
        const T_partials_return beta_dbl = value_of(beta_vec[n]);

        if (include_summand<propto,T_shape>::value)
          logp -= lgamma_alpha[n];
        if (include_summand<propto,T_shape,T_scale>::value)
          logp += alpha_dbl * log_beta[n];
        if (include_summand<propto,T_y,T_shape>::value)
          logp -= (alpha_dbl+1.0) * log_y[n];
        if (include_summand<propto,T_y,T_scale>::value)
          logp -= beta_dbl * inv_y[n];
  
        // gradients
        if (!is_constant<typename is_vector<T_y>::type>::value)
          operands_and_partials.d_x1[n] 
            += -(alpha_dbl+1) * inv_y[n] + beta_dbl * inv_y[n] * inv_y[n];
        if (!is_constant<typename is_vector<T_shape>::type>::value)
          operands_and_partials.d_x2[n] 
            += -digamma_alpha[n] + log_beta[n] - log_y[n];
        if (!is_constant<typename is_vector<T_scale>::type>::value)
          operands_and_partials.d_x3[n] += alpha_dbl / beta_dbl - inv_y[n];
      }
      return operands_and_partials.to_var(logp,y,alpha,beta);
    }

    template <typename T_y, typename T_shape, typename T_scale>
    inline
    typename return_type<T_y,T_shape,T_scale>::type
    inv_gamma_log(const T_y& y, const T_shape& alpha, const T_scale& beta) {
      return inv_gamma_log<false>(y,alpha,beta);
    }

    /**
     * The CDF of an inverse gamma density for y with the specified
     * shape and scale parameters. y, shape, and scale parameters must
     * be greater than 0.
     * 
     * @param y A scalar variable.
     * @param alpha Shape parameter.
     * @param beta Scale parameter.
     * @throw std::domain_error if alpha is not greater than 0.
     * @throw std::domain_error if beta is not greater than 0.
     * @throw std::domain_error if y is not greater than 0.
     * @tparam T_y Type of scalar.
     * @tparam T_shape Type of shape.
     * @tparam T_scale Type of scale.
     */
      
    template <typename T_y, typename T_shape, typename T_scale>
    typename return_type<T_y,T_shape,T_scale>::type
    inv_gamma_cdf(const T_y& y, const T_shape& alpha, const T_scale& beta) { 
      typedef typename stan::partials_return_type<T_y,T_shape,T_scale>::type
        T_partials_return;

      // Size checks
      if (!(stan::length(y) && stan::length(alpha) && stan::length(beta))) 
        return 1.0;
          
      // Error checks
      static const std::string function("stan::prob::inv_gamma_cdf");
          
      using stan::error_handling::check_positive_finite;      
      using stan::error_handling::check_not_nan;
      using stan::error_handling::check_consistent_sizes;
      using stan::error_handling::check_greater_or_equal;
      using stan::error_handling::check_less_or_equal;
      using stan::error_handling::check_nonnegative;
      using stan::math::value_of;
      using boost::math::tools::promote_args;
          
      T_partials_return P(1.0);
          
      check_positive_finite(function, "Shape parameter", alpha);
      check_positive_finite(function, "Scale parameter", beta);
      check_not_nan(function, "Random variable", y);
      check_nonnegative(function, "Random variable", y); 
      check_consistent_sizes(function, 
                             "Random variable", y, 
                             "Shape parameter", alpha, 
                             "Scale Parameter", beta);

      // Wrap arguments in vectors
      VectorView<const T_y> y_vec(y);
      VectorView<const T_shape> alpha_vec(alpha);
      VectorView<const T_scale> beta_vec(beta);
      size_t N = max_size(y, alpha, beta);
          
      agrad::OperandsAndPartials<T_y, T_shape, T_scale> 
        operands_and_partials(y, alpha, beta);
          
      // Explicit return for extreme values
      // The gradients are technically ill-defined, but treated as zero
          
      for (size_t i = 0; i < stan::length(y); i++) {
        if (value_of(y_vec[i]) == 0) 
          return operands_and_partials.to_var(0.0,y,alpha,beta);
      }
          
      // Compute CDF and its gradients
      using stan::math::gamma_q;
      using stan::math::digamma;
      using boost::math::tgamma;
      using std::exp;
      using std::pow;
          
      // Cache a few expensive function calls if nu is a parameter
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> gamma_vec(stan::length(alpha));
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return,T_shape> digamma_vec(stan::length(alpha));
          
      if (!is_constant_struct<T_shape>::value) {
        for (size_t i = 0; i < stan::length(alpha); i++) {
          const T_partials_return alpha_dbl = value_of(alpha_vec[i]);
          gamma_vec[i] = tgamma(alpha_dbl);
          digamma_vec[i] = digamma(alpha_dbl);
        }
      }
          
      // Compute vectorized CDF and gradient
      for (size_t n = 0; n < N; n++) {
        // Explicit results for extreme values
        // The gradients are technically ill-defined, but treated as zero
        if (value_of(y_vec[n]) == std::numeric_limits<double>::infinity())
          continue;
              
        // Pull out values
        const T_partials_return y_dbl = value_of(y_vec[n]);
        const T_partials_return y_inv_dbl = 1.0 / y_dbl;
        const T_partials_return alpha_dbl = value_of(alpha_vec[n]);
        const T_partials_return beta_dbl = value_of(beta_vec[n]);
              
        // Compute
        const T_partials_return Pn = gamma_q(alpha_dbl, beta_dbl * y_inv_dbl);
              
        P *= Pn;
              
        if (!is_constant_struct<T_y>::value)
          operands_and_partials.d_x1[n] += beta_dbl * y_inv_dbl * y_inv_dbl
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl 
                                               * y_inv_dbl,alpha_dbl-1) 
            / tgamma(alpha_dbl) / Pn;
        if (!is_constant_struct<T_shape>::value)
          operands_and_partials.d_x2[n] 
            += stan::math::grad_reg_inc_gamma(alpha_dbl, beta_dbl
                                              * y_inv_dbl, gamma_vec[n],
                                              digamma_vec[n]) / Pn;
        if (!is_constant_struct<T_scale>::value)
          operands_and_partials.d_x3[n] += - y_inv_dbl 
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl * y_inv_dbl,alpha_dbl-1)
            / tgamma(alpha_dbl) / Pn;
      }
          
      if (!is_constant_struct<T_y>::value)
        for (size_t n = 0; n < stan::length(y); ++n) 
          operands_and_partials.d_x1[n] *= P;
      if (!is_constant_struct<T_shape>::value)
        for (size_t n = 0; n < stan::length(alpha); ++n) 
          operands_and_partials.d_x2[n] *= P;
      if (!is_constant_struct<T_scale>::value)
        for (size_t n = 0; n < stan::length(beta); ++n) 
          operands_and_partials.d_x3[n] *= P;
          
      return operands_and_partials.to_var(P,y,alpha,beta);
    }

    template <typename T_y, typename T_shape, typename T_scale>
    typename return_type<T_y,T_shape,T_scale>::type
    inv_gamma_cdf_log(const T_y& y, const T_shape& alpha, 
                      const T_scale& beta) { 
      typedef typename stan::partials_return_type<T_y,T_shape,T_scale>::type
        T_partials_return;
    
      // Size checks
      if (!(stan::length(y) && stan::length(alpha) && stan::length(beta))) 
        return 0.0;
          
      // Error checks
      static const std::string function("stan::prob::inv_gamma_cdf_log");
          
      using stan::error_handling::check_positive_finite;      
      using stan::error_handling::check_not_nan;
      using stan::error_handling::check_consistent_sizes;
      using stan::error_handling::check_greater_or_equal;
      using stan::error_handling::check_less_or_equal;
      using stan::error_handling::check_nonnegative;
      using stan::math::value_of;
      using boost::math::tools::promote_args;
          
      T_partials_return P(0.0);
          
      check_positive_finite(function, "Shape parameter", alpha);
      check_positive_finite(function, "Scale parameter", beta);
      check_not_nan(function, "Random variable", y);
      check_nonnegative(function, "Random variable", y);
      check_consistent_sizes(function,
                             "Random variable", y, 
                             "Shape parameter", alpha, 
                             "Scale Parameter", beta);
          
      // Wrap arguments in vectors
      VectorView<const T_y> y_vec(y);
      VectorView<const T_shape> alpha_vec(alpha);
      VectorView<const T_scale> beta_vec(beta);
      size_t N = max_size(y, alpha, beta);
          
      agrad::OperandsAndPartials<T_y, T_shape, T_scale> 
        operands_and_partials(y, alpha, beta);
          
      // Explicit return for extreme values
      // The gradients are technically ill-defined, but treated as zero
          
      for (size_t i = 0; i < stan::length(y); i++) {
        if (value_of(y_vec[i]) == 0) 
          return operands_and_partials.to_var(stan::math::negative_infinity(),
                                              y,alpha,beta);
      }
          
      // Compute cdf_log and its gradients
      using stan::math::gamma_q;
      using stan::math::digamma;
      using boost::math::tgamma;
      using std::exp;
      using std::pow;
          
      // Cache a few expensive function calls if nu is a parameter
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> gamma_vec(stan::length(alpha));
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> 
        digamma_vec(stan::length(alpha));
          
      if (!is_constant_struct<T_shape>::value) {
        for (size_t i = 0; i < stan::length(alpha); i++) {
          const T_partials_return alpha_dbl = value_of(alpha_vec[i]);
          gamma_vec[i] = tgamma(alpha_dbl);
          digamma_vec[i] = digamma(alpha_dbl);
        }
      }
          
      // Compute vectorized cdf_log and gradient
      for (size_t n = 0; n < N; n++) {
        // Explicit results for extreme values
        // The gradients are technically ill-defined, but treated as zero
        if (value_of(y_vec[n]) == std::numeric_limits<double>::infinity())
          continue;
              
        // Pull out values
        const T_partials_return y_dbl = value_of(y_vec[n]);
        const T_partials_return y_inv_dbl = 1.0 / y_dbl;
        const T_partials_return alpha_dbl = value_of(alpha_vec[n]);
        const T_partials_return beta_dbl = value_of(beta_vec[n]);
              
        // Compute
        const T_partials_return Pn = gamma_q(alpha_dbl, beta_dbl * y_inv_dbl);
              
        P += log(Pn);
              
        if (!is_constant_struct<T_y>::value)
          operands_and_partials.d_x1[n] += beta_dbl * y_inv_dbl * y_inv_dbl 
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl * y_inv_dbl,
                                               alpha_dbl-1) 
            / tgamma(alpha_dbl) / Pn;
        if (!is_constant_struct<T_shape>::value)
          operands_and_partials.d_x2[n] 
            += stan::math::grad_reg_inc_gamma(alpha_dbl, beta_dbl
                                              * y_inv_dbl, gamma_vec[n],
                                              digamma_vec[n]) / Pn;
        if (!is_constant_struct<T_scale>::value)
          operands_and_partials.d_x3[n] += - y_inv_dbl 
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl * y_inv_dbl,alpha_dbl-1)
            / tgamma(alpha_dbl) / Pn;
      }
          
      return operands_and_partials.to_var(P,y,alpha,beta);
    }
      
    template <typename T_y, typename T_shape, typename T_scale>
    typename return_type<T_y,T_shape,T_scale>::type
    inv_gamma_ccdf_log(const T_y& y, const T_shape& alpha,
                       const T_scale& beta) { 
      typedef typename stan::partials_return_type<T_y,T_shape,T_scale>::type
        T_partials_return;

      // Size checks
      if (!(stan::length(y) && stan::length(alpha) && stan::length(beta))) 
        return 0.0;
          
      // Error checks
      static const std::string function("stan::prob::inv_gamma_ccdf_log");
          
      using stan::error_handling::check_positive_finite;      
      using stan::error_handling::check_not_nan;
      using stan::error_handling::check_consistent_sizes;
      using stan::error_handling::check_greater_or_equal;
      using stan::error_handling::check_less_or_equal;
      using stan::error_handling::check_nonnegative;
      using stan::math::value_of;
      using boost::math::tools::promote_args;
          
      T_partials_return P(0.0);
          
      check_positive_finite(function, "Shape parameter", alpha);
      check_positive_finite(function, "Scale parameter", beta);
      check_not_nan(function, "Random variable", y);
      check_nonnegative(function, "Random variable", y);
      check_consistent_sizes(function, 
                             "Random variable", y, 
                             "Shape parameter", alpha, 
                             "Scale Parameter", beta);
          
      // Wrap arguments in vectors
      VectorView<const T_y> y_vec(y);
      VectorView<const T_shape> alpha_vec(alpha);
      VectorView<const T_scale> beta_vec(beta);
      size_t N = max_size(y, alpha, beta);
          
      agrad::OperandsAndPartials<T_y, T_shape, T_scale> 
        operands_and_partials(y, alpha, beta);
          
      // Explicit return for extreme values
      // The gradients are technically ill-defined, but treated as zero
          
      for (size_t i = 0; i < stan::length(y); i++) {
        if (value_of(y_vec[i]) == 0) 
          return operands_and_partials.to_var(0.0,y,alpha,beta);
      }
          
      // Compute ccdf_log and its gradients
      using stan::math::gamma_q;
      using stan::math::digamma;
      using boost::math::tgamma;
      using std::exp;
      using std::pow;
          
      // Cache a few expensive function calls if nu is a parameter
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> gamma_vec(stan::length(alpha));
      VectorBuilder<!is_constant_struct<T_shape>::value,
                    T_partials_return, T_shape> 
        digamma_vec(stan::length(alpha));
          
      if (!is_constant_struct<T_shape>::value) {
        for (size_t i = 0; i < stan::length(alpha); i++) {
          const T_partials_return alpha_dbl = value_of(alpha_vec[i]);
          gamma_vec[i] = tgamma(alpha_dbl);
          digamma_vec[i] = digamma(alpha_dbl);
        }
      }
          
      // Compute vectorized ccdf_log and gradient
      for (size_t n = 0; n < N; n++) {
        // Explicit results for extreme values
        // The gradients are technically ill-defined, but treated as zero
        if (value_of(y_vec[n]) == std::numeric_limits<double>::infinity())
          return operands_and_partials.to_var(stan::math::negative_infinity(),
                                              y,alpha,beta);
              
        // Pull out values
        const T_partials_return y_dbl = value_of(y_vec[n]);
        const T_partials_return y_inv_dbl = 1.0 / y_dbl;
        const T_partials_return alpha_dbl = value_of(alpha_vec[n]);
        const T_partials_return beta_dbl = value_of(beta_vec[n]);
              
        // Compute
        const T_partials_return Pn = 1.0 - gamma_q(alpha_dbl, beta_dbl 
                                                   * y_inv_dbl);
              
        P += log(Pn);
              
        if (!is_constant_struct<T_y>::value)
          operands_and_partials.d_x1[n] -= beta_dbl * y_inv_dbl * y_inv_dbl 
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl * y_inv_dbl,
                                               alpha_dbl-1) 
            / tgamma(alpha_dbl) / Pn;
        if (!is_constant_struct<T_shape>::value)
          operands_and_partials.d_x2[n] 
            -= stan::math::grad_reg_inc_gamma(alpha_dbl, beta_dbl
                                              * y_inv_dbl, gamma_vec[n],
                                              digamma_vec[n]) / Pn;
        if (!is_constant_struct<T_scale>::value)
          operands_and_partials.d_x3[n] += y_inv_dbl 
            * exp(-beta_dbl * y_inv_dbl) * pow(beta_dbl * y_inv_dbl,alpha_dbl-1)
            / tgamma(alpha_dbl) / Pn;
      }
          
      return operands_and_partials.to_var(P,y,alpha,beta);
    }

    template <class RNG>
    inline double
    inv_gamma_rng(const double alpha,
                  const double beta,
                  RNG& rng) {
      using boost::variate_generator;
      using boost::random::gamma_distribution;

      static const std::string function("stan::prob::inv_gamma_rng");

      using stan::error_handling::check_positive_finite;
 
      check_positive_finite(function, "Shape parameter", alpha);
      check_positive_finite(function, "Scale parameter", beta);

      variate_generator<RNG&, gamma_distribution<> >
        gamma_rng(rng, gamma_distribution<>(alpha, 1 / beta));
      return 1 / gamma_rng();
    }          
  }
}

#endif
