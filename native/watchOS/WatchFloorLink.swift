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
    @Published private(set) var selectedStage: AudienceClient.Venue?
    private var whenJoined: (() -> Void)?
    private var phoneDeadline: Task<Void, Never>?
    private var ending = false

    #if DEBUG && targetEnvironment(simulator)
    static let interruptedStageFixture = ProcessInfo.processInfo.arguments.contains("--preview-audience-reconnection")
    private var fixtureSeeded = false

    func seedInterruptedStageFixture() {
        guard Self.interruptedStageFixture, !fixtureSeeded else { return }
        fixtureSeeded = true
        route = .nearbyStage
        selectedStage = stage.previewInterruptedStage()
    }

    var interruptedStageDiagnostics: String {
        "route=\(route) reconnecting=\(stage.reconnecting) connecting=\(stage.connecting) " +
        "joined=\(stage.joined) phoneJoined=\(phoneJoined) relay=\(phone.status)"
    }
    #endif

    init(phone: WatchRelay) {
        self.phone = phone
        stage.onJoin = { [weak self] in
            let ready = self?.whenJoined
            self?.whenJoined = nil
            ready?()
        }
        stage.onEnd = { [weak self] in
            guard let self, !self.ending, self.route == .nearbyStage else { return }
            self.interrupted?(self.stage.status)
        }
        #if DEBUG && targetEnvironment(simulator)
        // The cancellation fixture renders a simulated stage with an inert relay.
        if Self.interruptedStageFixture { return }
        #endif
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
        guard route == .nearbyStage, !phoneJoined else { return }
        ending = true
        selectedStage = nil
        stage.discover(room: nil)
        ending = false
    }

    func choose(_ venue: AudienceClient.Venue) {
        guard route == .nearbyStage, stage.looking else { return }
        stage.stopBrowsing()
        selectedStage = venue
    }

    var canStart: Bool { route.ready(phoneReachable: phone.reachable, stageJoined: selectedStage != nil) }
    var ready: Bool { route.ready(phoneReachable: phone.reachable, stageJoined: stage.joined) }

    func begin(whenReady: @escaping () -> Void) {
        switch route {
        case .pairedPhone:
            guard phone.reachable else { interrupted?("iPhone connection lost. Rejoin the floor."); return }
            phone.beginAudience()
            whenReady()
        case .nearbyStage:
            guard let selectedStage else { interrupted?("Choose a nearby stage before sharing."); return }
            whenJoined = whenReady
            // The stage handshake has an idle limit. Open it only once permissions are settled.
            if phone.reachable { checkPhoneBeforeJoining(selectedStage.id) }
            else { joinStage(selectedStage.id) }
        }
    }

    private func checkPhoneBeforeJoining(_ stageID: UUID) {
        let token = UUID()
        verifying = token
        phoneDeadline?.cancel()
        phoneDeadline = Task { [weak self] in
            do { try await Task.sleep(for: .seconds(2)) } catch { return }
            guard let self, self.verifying == token else { return }
            self.interrupted?("Could not check your iPhone. Try again or choose Paired iPhone.")
        }
        phone.checkPhoneFloor { [weak self] active in
            guard let self, self.verifying == token, self.route == .nearbyStage else { return }
            self.verifying = nil
            self.phoneDeadline?.cancel()
            self.phoneJoined = active
            if active {
                self.select(.pairedPhone)
                self.interrupted?("Your iPhone is already on the floor. Start with Paired iPhone.")
            } else { self.joinStage(stageID) }
        }
    }

    private func joinStage(_ id: UUID) {
        if !stage.join(id) { interrupted?("Stage unavailable. Find your stage again to rejoin.") }
    }

    func send(_ sample: AudienceSample, motionAge: Double) {
        switch route {
        case .pairedPhone: phone.sendAudience(sample, motionAge: motionAge)
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
        whenJoined = nil
        if selectedStage != nil || stage.looking || stage.connecting || stage.joined { stage.leave() }
        selectedStage = nil
        ending = false
    }
}
