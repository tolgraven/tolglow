import Foundation
import SwiftUI
import TolglowSensorCore

enum StudioOperatorAccess {
    static let service = "org.tolglow.studio.operator"

    struct SignInRequired: LocalizedError {
        var errorDescription: String? { "Save the operator password for this Studio in Connect and try again." }
    }

    static func checkSignIn(_ response: URLResponse) throws {
        if (response as? HTTPURLResponse)?.statusCode == 401 { throw SignInRequired() }
    }

    static func respond(to challenge: URLAuthenticationChallenge, requestURL: URL?,
                        completion: (URLSession.AuthChallengeDisposition, URLCredential?) -> Void) {
        let space = challenge.protectionSpace
        guard space.authenticationMethod == NSURLAuthenticationMethodHTTPBasic else {
            completion(.performDefaultHandling, nil)
            return
        }
        guard let url = requestURL, let requested = StudioOrigin(url: url),
              requested.acceptsOperatorChallenge(scheme: space.protocol, host: space.host, port: space.port,
                                                 realm: space.realm, isProxy: space.proxyType != nil,
                                                 previousFailures: challenge.previousFailureCount),
              let token = LocalPassword.read(service: service, account: requested.keychainAccount) else {
            completion(.rejectProtectionSpace, nil)
            return
        }
        completion(.useCredential, URLCredential(user: "studio", password: token, persistence: .none))
    }
}

struct StudioOperatorAccessSection: View {
    @AppStorage("studio-origin") private var address = "http://tolglow.local:8500"
    @State private var password = ""
    @State private var status = ""
    private var destination: StudioOrigin? {
        guard let url = URL(string: address), url.query == nil, url.fragment == nil,
              url.path.isEmpty || url.path == "/" else { return nil }
        return StudioOrigin(url: url)
    }

    var body: some View {
        Section("Studio operator") {
            TextField("HTTPS Studio origin", text: $address)
                .keyboardType(.URL).textInputAutocapitalization(.never).autocorrectionDisabled()
                .onChange(of: address) { _, _ in password = ""; status = "" }
            SecureField("Operator password", text: $password).textContentType(.password)
            Button("Save operator password") {
                guard let destination else { return }
                do {
                    try LocalPassword.save(password, service: StudioOperatorAccess.service, account: destination.keychainAccount)
                    password = ""
                    status = "Password saved for this Studio in Keychain"
                } catch { status = "Could not save the Studio password" }
            }.disabled(destination == nil || !(24...1024).contains(password.utf16.count))
            Button("Forget saved password") {
                guard let destination else { return }
                do {
                    try LocalPassword.remove(service: StudioOperatorAccess.service, account: destination.keychainAccount)
                    password = ""
                    status = "Saved password removed for this Studio"
                } catch { status = "Could not remove the Studio password" }
            }.disabled(destination == nil)
            if !status.isEmpty { Text(status).font(.caption).foregroundStyle(.secondary) }
        }
    }
}
