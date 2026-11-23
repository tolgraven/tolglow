public enum AudienceWatchRoute: String, CaseIterable {
    case pairedPhone, nearbyStage

    public func ready(phoneReachable: Bool, stageJoined: Bool) -> Bool {
        switch self {
        case .pairedPhone: return phoneReachable
        case .nearbyStage: return stageJoined
        }
    }
}
