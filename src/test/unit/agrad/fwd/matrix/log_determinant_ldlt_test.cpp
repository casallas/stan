#include <gtest/gtest.h>
#include <stan/agrad/fwd/matrix/typedefs.hpp>
#include <stan/math/matrix/log_determinant_ldlt.hpp>
#include <stan/math/matrix/LDLT_factor.hpp>
#include <stan/agrad/fwd.hpp>
#include <stan/agrad/rev.hpp>
#include <test/unit/agrad/util.hpp>

TEST(AgradFwdMatrixLogDeterminantLDLT,fd) {
  using stan::agrad::matrix_fd;
  using stan::agrad::fvar;
  using stan::math::log_determinant_ldlt;
  
  matrix_fd v(2,2);
  v << 3, 0, 0, 4;
  v(0,0).d_ = 1.0;
  v(0,1).d_ = 2.0;
  v(1,0).d_ = 2.0;
  v(1,1).d_ = 2.0;
  
  stan::math::LDLT_factor<fvar<double>,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<double> det;
  det = log_determinant_ldlt(ldlt_v);
  EXPECT_FLOAT_EQ(std::log(12.0), det.val_);
  EXPECT_FLOAT_EQ(0.83333333, det.d_);
}

TEST(AgradFwdMatrixLogDeterminantLDLT,fv_1stDeriv) {
  using stan::agrad::matrix_fv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<var> a(3.0,1.0);
  fvar<var> b(0.0,2.0);
  fvar<var> c(0.0,2.0);
  fvar<var> d(4.0,2.0);

  matrix_fv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<var>,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<var> det;
  det = log_determinant_ldlt(ldlt_v);
  EXPECT_FLOAT_EQ(std::log(12.0), det.val_.val());
  EXPECT_FLOAT_EQ(0.83333333, det.d_.val());

  AVEC q = createAVEC(a.val(),b.val(),c.val(),d.val());
  VEC h;
  det.val_.grad(q,h);
  EXPECT_FLOAT_EQ(0.33333333,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(0,h[2]);
  EXPECT_FLOAT_EQ(0.25,h[3]);
}
TEST(AgradFwdMatrixLogDeterminantLDLT,fv_2ndDeriv) {
  using stan::agrad::matrix_fv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<var> a(3.0,1.0);
  fvar<var> b(0.0,2.0);
  fvar<var> c(0.0,2.0);
  fvar<var> d(4.0,2.0);
  matrix_fv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<var>,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<var> det;
  det = log_determinant_ldlt(ldlt_v);

  AVEC q = createAVEC(a.val(),b.val(),c.val(),d.val());
  VEC h;
  det.d_.grad(q,h);
  EXPECT_FLOAT_EQ(-0.11111111,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(-0.33333333,h[2]);
  EXPECT_FLOAT_EQ(-0.125,h[3]);
}
TEST(AgradFwdMatrixLogDeterminantLDLT,ffd) {
  using stan::agrad::matrix_ffd;
  using stan::agrad::fvar;
  using stan::math::log_determinant_ldlt;
  
  fvar<fvar<double> > a,b,c,d;
  a.val_.val_ = 3.0;
  a.d_.val_ = 1.0;  
  b.val_.val_ = 0.0;
  b.d_.val_ = 2.0;
  c.val_.val_ = 0.0;
  c.d_.val_ = 2.0;
  d.val_.val_ = 4.0;
  d.d_.val_ = 2.0; 

  matrix_ffd v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<fvar<double> >,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<fvar<double> > det;
  det = log_determinant_ldlt(ldlt_v);
  EXPECT_FLOAT_EQ(std::log(12.0), det.val_.val());
  EXPECT_FLOAT_EQ(0.83333333, det.d_.val());
}

TEST(AgradFwdMatrixLogDeterminantLDLT,ffv_1stDeriv) {
  using stan::agrad::matrix_ffv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<fvar<var> > a(3.0,1.0);
  fvar<fvar<var> > b(0.0,2.0);
  fvar<fvar<var> > c(0.0,2.0);
  fvar<fvar<var> > d(4.0,2.0);

  matrix_ffv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<fvar<var> >,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<fvar<var> > det;
  det = log_determinant_ldlt(ldlt_v);
  EXPECT_FLOAT_EQ(std::log(12.0), det.val_.val().val());
  EXPECT_FLOAT_EQ(0.83333333, det.d_.val().val());

  AVEC q = createAVEC(a.val().val(),b.val().val(),c.val().val(),d.val().val());
  VEC h;
  det.val_.val().grad(q,h);
  EXPECT_FLOAT_EQ(0.33333333,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(0,h[2]);
  EXPECT_FLOAT_EQ(0.25,h[3]);
}
TEST(AgradFwdMatrixLogDeterminantLDLT,ffv_2ndDeriv_1) {
  using stan::agrad::matrix_ffv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<fvar<var> > a(3.0,1.0);
  fvar<fvar<var> > b(0.0,2.0);
  fvar<fvar<var> > c(0.0,2.0);
  fvar<fvar<var> > d(4.0,2.0);
  a.val_.d_ = 1.0;
  b.val_.d_ = 1.0;
  c.val_.d_ = 1.0;
  d.val_.d_ = 1.0;
  matrix_ffv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<fvar<var> >,-1,-1> ldlt_v;
  ldlt_v.compute(v);
  
  fvar<fvar<var> > det;
  det = log_determinant_ldlt(ldlt_v);

  AVEC q = createAVEC(a.val().val(),b.val().val(),c.val().val(),d.val().val());
  VEC h;
  det.val().d_.grad(q,h);
  EXPECT_FLOAT_EQ(-0.11111111,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(-0.16666667,h[2]);
  EXPECT_FLOAT_EQ(-0.0625,h[3]);
}
TEST(AgradFwdMatrixLogDeterminantLDLT,ffv_2ndDeriv_2) {
  using stan::agrad::matrix_ffv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<fvar<var> > a(3.0,1.0);
  fvar<fvar<var> > b(0.0,2.0);
  fvar<fvar<var> > c(0.0,2.0);
  fvar<fvar<var> > d(4.0,2.0);
  matrix_ffv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<fvar<var> >,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<fvar<var> > det;
  det = log_determinant_ldlt(ldlt_v);

  AVEC q = createAVEC(a.val().val(),b.val().val(),c.val().val(),d.val().val());
  VEC h;
  det.d_.val().grad(q,h);
  EXPECT_FLOAT_EQ(-0.11111111,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(-0.33333333,h[2]);
  EXPECT_FLOAT_EQ(-.125,h[3]);
}
TEST(AgradFwdMatrixLogDeterminantLDLT,ffv_3rdDeriv) {
  using stan::agrad::matrix_ffv;
  using stan::agrad::fvar;
  using stan::agrad::var;
  using stan::math::log_determinant_ldlt;
  
  fvar<fvar<var> > a(3.0,1.0);
  fvar<fvar<var> > b(0.0,2.0);
  fvar<fvar<var> > c(0.0,2.0);
  fvar<fvar<var> > d(4.0,2.0);
  a.val_.d_ = 1.0;
  b.val_.d_ = 1.0;
  c.val_.d_ = 1.0;
  d.val_.d_ = 1.0;

  matrix_ffv v(2,2);
  v << a,b,c,d;
  
  stan::math::LDLT_factor<fvar<fvar<var> >,-1,-1> ldlt_v;
  ldlt_v.compute(v);

  fvar<fvar<var> > det;
  det = log_determinant_ldlt(ldlt_v);

  AVEC q = createAVEC(a.val().val(),b.val().val(),c.val().val(),d.val().val());
  VEC h;
  det.d_.d_.grad(q,h);
  EXPECT_FLOAT_EQ(0.18518518,h[0]);
  EXPECT_FLOAT_EQ(0,h[1]);
  EXPECT_FLOAT_EQ(0.33333333,h[2]);
  EXPECT_FLOAT_EQ(0.14583333,h[3]);
}
