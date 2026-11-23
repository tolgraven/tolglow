import SwiftUI

@main struct TolglowAudienceClip: App {
    @StateObject private var session = AudienceSession()
    @State private var room: String?
    @State private var invalidInvite = false
    var body: some Scene {
        WindowGroup {
            Group {
                if invalidInvite { ContentUnavailableView("Invalid stage invitation", systemImage: "qrcode.viewfinder") }
                else { AudienceView(session: session, room: room) }
            }
            .onContinueUserActivity(NSUserActivityTypeBrowsingWeb) { activity in
                room = activity.webpageURL.flatMap(AudienceRadio.room)
                invalidInvite = room == nil
                session.client.leave()
            }.preferredColorScheme(.dark)
        }
    }
}
