import Combine
import Foundation
import TolglowSensorCore

/// One wrist, one output. A failed send never falls through to a second radio.
@MainActor final class WatchFloorLink: ObservableObject {
    let stage = AudienceClient()
    private let phone: WatchRelay
    @Published private(set) var route = AudienceWatchRoute.pairedPhone
    @Published private(set) var phoneJoined = false
    var interrupted: ((String) -> Void)?
    private var verifying: UUID?
    private var phoneDeadline: Task<Void, Never>?
    private var ending = false

    init(phone: WatchRelay) {
        self.phone = phone
        stage.onEnd = { [weak self] in
            guard let self, !self.ending, self.route == .nearbyStage else { return }
            self.interrupted?("Stage disconnected. Find your stage again to rejoin.")
        }
        phone.phoneFloorChanged = { [weak self] active in
            guard let self else { return }
            self.phoneJoined = active
            if !self.route.available(phoneJoined: active) {
                self.select(.pairedPhone)
                self.interrupted?("Your iPhone joined the floor. Use Paired iPhone to share once.")
            }
        }
    }

    func select(_ next: AudienceWatchRoute) {
        guard next != route, next.available(phoneJoined: phoneJoined) else { return }
        end()
        route = next
    }

    func discover() {
        guard route == .nearbyStage else { return }
        if phone.reachable {
            // Confirm the paired phone has no active membership before opening a second one.
            let token = UUID()
            verifying = token
            phoneDeadline?.cancel()
            phoneDeadline = Task { [weak self] in
                do { try await Task.sleep(for: .seconds(2)) } catch { return }
                guard let self, self.verifying == token else { return }
                self.verifying = nil
                self.interrupted?("Could not check your iPhone. Try again or choose Paired iPhone.")
            }
            phone.checkPhoneFloor { [weak self] active in
                guard let self, self.verifying == token, self.route == .nearbyStage else { return }
                self.verifying = nil
                self.phoneDeadline?.cancel()
                if active {
                    self.phoneJoined = true
                    self.select(.pairedPhone)
                    self.interrupted?("Your iPhone is already on the floor. Start with Paired iPhone.")
                } else { self.scan() }
            }
        } else { scan() }
    }

    private func scan() {
        ending = true
        stage.discover(room: nil)
        ending = false
    }

    var ready: Bool { route.ready(phoneReachable: phone.reachable, stageJoined: stage.joined) }

    func begin() {
        if route == .pairedPhone { phone.beginAudience() }
    }

    func send(_ sample: AudienceSample) {
        switch route {
        case .pairedPhone: phone.sendAudience(sample)
        case .nearbyStage: stage.offer(sample)
        }
    }

    func end() {
        guard !ending else { return }
        ending = true
        verifying = nil
        phoneDeadline?.cancel()
        phoneDeadline = nil
        phone.endAudience()
        if stage.looking || stage.connecting || stage.joined { stage.leave() }
        ending = false
    }
}
