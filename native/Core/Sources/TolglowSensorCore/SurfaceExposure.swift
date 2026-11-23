import Foundation

/// Pair one dark and one illuminated exposure at the same marked point, carrying
/// the host's exact presentation receipt across each camera acquisition.
public enum SurfaceExposure {
    @MainActor public static func pair<Receipt>(
        present: (String) async throws -> Receipt,
        frames: () async throws -> [Double],
        verifyStance: () throws -> Void,
        record: (Int, Receipt, [Double]) async throws -> Void
    ) async throws {
        for (offset, phase) in ["dark", "white"].enumerated() {
            let receipt = try await present(phase)
            try verifyStance()
            let levels = try await frames()
            try Task.checkCancellation()
            try verifyStance()
            _ = try PhotometryFrame.stableMedian(levels)
            try await record(offset, receipt, levels)
        }
    }
}
