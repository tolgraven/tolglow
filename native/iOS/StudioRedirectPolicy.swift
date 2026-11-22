import Foundation

/// API mutations and calibration data belong to the origin the operator entered.
final class StudioRedirectPolicy: NSObject, URLSessionTaskDelegate {
    static let shared = StudioRedirectPolicy()

    func urlSession(_ session: URLSession, task: URLSessionTask,
                    willPerformHTTPRedirection response: HTTPURLResponse,
                    newRequest request: URLRequest,
                    completionHandler: @escaping (URLRequest?) -> Void) {
        completionHandler(nil)
    }
}
