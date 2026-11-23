import SwiftUI

@main struct TolglowAudienceClip: App {
    @StateObject private var session = AudienceSession()

    var body: some Scene {
        WindowGroup {
            AudienceView(session: session)
                .onContinueUserActivity(NSUserActivityTypeBrowsingWeb) { activity in
                    session.client.receiveInvitation(activity.webpageURL.flatMap(AudienceRadio.room))
                }
                .preferredColorScheme(.dark)
        }
    }
}
