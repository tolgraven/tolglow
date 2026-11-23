public enum AudienceWatchRoute: String, CaseIterable {
    case pairedPhone, nearbyStage

    public func available(phoneJoined: Bool) -> Bool {
        self == .pairedPhone || !phoneJoined
    }

    public func ready(phoneReachable: Bool, stageJoined: Bool) -> Bool {
        switch self {
        case .pairedPhone: return phoneReachable
        case .nearbyStage: return stageJoined
        }
    }
}
