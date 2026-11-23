import unittest

import budget


class HardwareBudgetTest(unittest.TestCase):
    def test_calibration_is_time_weighted(self):
        self.assertEqual(30, budget.calibrated_rate([(50, 1), (100, 4)]))
        for window in [[(0, 1)], [(2.5, 1)], [(1, 0)], [(1, float("nan"))], []]:
            with self.subTest(window=window), self.assertRaises(ValueError):
                budget.calibrated_rate(window)

    def test_epochs_setup_validation_and_whole_instance_charge(self):
        result = budget.estimate(budget.HARDWARE["gb200-node"], "7B", 3600, 3,
                                 rates=(3, 6), setup_minutes=30, overhead=0.5)
        self.assertEqual([1.25, 2.0], result["elapsed_hours"])
        self.assertEqual([52.5, 84.0], result["whole_instance_usd"])
        self.assertEqual(10800, result["processed_tokens"])

    def test_challenger_accepts_separate_measured_rate(self):
        result = budget.estimate(budget.HARDWARE["m4-pro-48gb"], "Qwen3.5-4B", 3600,
                                 rates=(1, 2))
        self.assertEqual("supplied", result["rate_source"])
        self.assertEqual([0.5, 1.0], result["elapsed_hours"])

    def test_power_and_billing_rounding(self):
        result = budget.estimate(budget.HARDWARE["h100-80gb"], "7B", 61, rates=(1, 1))
        self.assertAlmostEqual(4.89 * 2 / 60, result["whole_instance_usd"][0])
        local = budget.estimate(budget.HARDWARE["m4-pro-48gb"], "7B", 3600, rates=(1, 1),
                                watts=(20, 40), usd_kwh=0.5)
        self.assertEqual([0.01, 0.02], local["local_electricity_usd"])

    def test_rejects_unavailable_lanes_and_invalid_inputs(self):
        phone = budget.HARDWARE["iphone-8gb"]
        self.assertEqual("decode", budget.estimate(phone, "1.5B", 100)["task"])
        for size, opts in [("7B", {}), ("1.5B", {"epochs": 2}), ("1.5B", {"rates": (2, 1)}),
                           ("1.5B", {"rates": (0, 1)}), ("1.5B", {"overhead": -1}),
                           ("1.5B", {"usd_kwh": float("inf")}), ("1.5B", {"watts": (40, 20)})]:
            with self.subTest(size=size, opts=opts), self.assertRaises(ValueError):
                budget.estimate(phone, size, 100, **opts)


if __name__ == "__main__":
    unittest.main()
