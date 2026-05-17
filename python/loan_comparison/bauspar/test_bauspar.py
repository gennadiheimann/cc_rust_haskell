import unittest
from bauspar import Bauspar
from configuration import ConfigReader


class TestBauspar(unittest.TestCase):

  def test_calculate_saving_amount_with_additional_payment(self):
      config = ConfigReader("config_for_unittest.toml")
      bs = Bauspar(config)
      (total_interest, total_saving_amount) = bs.calculate_saving_amount_with_additional_payment()
      self.assertAlmostEqual(total_interest, 281.99, delta=0.01)
      self.assertAlmostEqual(total_saving_amount, 46381.99, delta=0.01)
  
  def test_calc_bsv_saving_time_to_reach_evaluation_score(self):
    config = ConfigReader("config_for_unittest.toml")
    bs = Bauspar(config)

    (total_interest, total_saving_amount, m, evaluation_score) = \
      bs.calc_bsv_saving_time_to_reach_evaluation_score()

    self.assertAlmostEqual(total_interest, 244.58, delta=0.01)
    self.assertAlmostEqual(total_saving_amount, 43844.58, delta=0.01)
    self.assertEqual(m, 146)
    self.assertAlmostEqual(evaluation_score, 215.74, delta=0.01)
        


if __name__ == "__main__":
    unittest.main()