#include <gtest/gtest.h>
#include <stan/prob/internal_math/math/grad_reg_inc_gamma.hpp>
#include <stan/agrad/fwd.hpp>
#include <stan/agrad/rev.hpp>
#include <test/unit/agrad/util.hpp>

TEST(ProbInternalMath, gradRegIncGamma_typical) {
  double a = 0.5;
  double b = 1.0;
  double g = 1.77245;
  double dig = -1.96351;
  
  EXPECT_FLOAT_EQ(0.38984156, stan::math::grad_reg_inc_gamma(a, b, g, dig));
}

TEST(ProbInternalMath, gradRegIncGamma_infLoopInVersion2_0_1) {
  double a = 8.01006;
  double b = 2.47579e+215;
  double g = 5143.28;
  double dig = 2.01698;
  
  EXPECT_THROW(stan::math::grad_reg_inc_gamma(a, b, g, dig),
               std::domain_error);
}
TEST(ProbInternalMath, gradRegIncGamma_fd) {
  using stan::agrad::fvar;

  fvar<double> a = 0.5;
  fvar<double> b = 1.0;
  fvar<double> g = 1.77245;
  fvar<double> dig = -1.96351;
  
  EXPECT_FLOAT_EQ(0.38984156, stan::math::grad_reg_inc_gamma(a, b, g, dig).val());
}
TEST(ProbInternalMath, gradRegIncGamma_ffd) {
  using stan::agrad::fvar;

  fvar<fvar<double> > a = 0.5;
  fvar<fvar<double> > b = 1.0;
  fvar<fvar<double> > g = 1.77245;
  fvar<fvar<double> > dig = -1.96351;
  
  EXPECT_FLOAT_EQ(0.38984156, stan::math::grad_reg_inc_gamma(a, b, g, dig).val_.val_);
}

TEST(ProbInternalMath, gradRegIncGamma_fv) {
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::agrad::digamma;

  fvar<var> a = 0.5;
  fvar<var> b = 1.0;
  fvar<var> g = 1.77245;
  fvar<var> dig = digamma(a);
  
  EXPECT_FLOAT_EQ(0.38984156, stan::math::grad_reg_inc_gamma(a, b, g, dig).val_.val());
}

TEST(ProbInternalMath, gradRegIncGamma_fv_1stderiv) {
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::agrad::digamma;
  using stan::agrad::tgamma;

  fvar<var> a = 0.5;
  a.d_ = 1.0;
  fvar<var> b = 1.0;
  fvar<var> g = tgamma(a);
  fvar<var> dig = digamma(a);
  a.d_ = 1.0;
  
  fvar<var> z = stan::math::grad_reg_inc_gamma(a, b, g, dig);

  AVEC y1 = createAVEC(a.val_);
  VEC grad1;
  z.val_.grad(y1,grad1);
  EXPECT_NEAR(0.2134999674954450667,grad1[0],1e-6);
}

TEST(ProbInternalMath, gradRegIncGamma_fv_2ndderiv) {
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::agrad::digamma;
  using stan::agrad::tgamma;

  fvar<var> a = 0.5;
  a.d_ = 1.0;
  fvar<var> b = 1.0;
  fvar<var> g = tgamma(a);
  fvar<var> dig = digamma(a);
  a.d_ = 1.0;
  
  fvar<var> z = stan::math::grad_reg_inc_gamma(a, b, g, dig);

  AVEC y1 = createAVEC(a.val_);
  VEC grad1;
  z.d_.grad(y1,grad1);
  EXPECT_NEAR(-0.546236927878295422,grad1[0],1e-6);
}
