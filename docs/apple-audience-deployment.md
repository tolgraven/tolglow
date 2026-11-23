# Deploying an audience invitation

A physical QR carries an HTTPS URL for the stage. Before printing invitations, associate your domain with the signed companion and its App Clip. The Mac stage creates the invitation locally for its running session.

## Open a stage from Studio

In the packaged Mac app, open **Connections → Stage invitation**. Start the OSC receiver and choose a sensor enrolled for the room. Enter its public HTTPS invocation origin and a stage name, then select **Start stage**. The invitation appears when the helper creates the stage key. **Discoverable** means the Mac has begun Bluetooth advertising. Select **Save QR image** to put that invitation on a screen or invitation card for this run. Arm the fresh sensor session through the normal show controls before mapping audience movement to lighting.

**Stop stage**, replacing its sensor credential, stopping OSC, or loading another show invalidates that QR. Starting again produces a new fingerprint. A guest with an older image needs the current invitation. A reusable venue invitation would require an operator-selected identity protected in Keychain with a fresh challenge on every connection. That option remains open.

## Associate the domain

1. Pick an HTTPS origin you operate, such as `https://lights.example.org`. Build the parent and Clip with `AUDIENCE_DOMAIN=lights.example.org`. Enable Associated Domains for those App IDs in the signing team.
2. Obtain the prefix of the signed `application-identifier` entitlement. This is commonly your ten-character Team ID. Generate the association document with that prefix from the repository root.

   ```sh
   mkdir -p site-root/.well-known
   native/Packaging/audience-association ABCDE12345 > site-root/.well-known/apple-app-site-association
   ```

3. Serve that file from `https://lights.example.org/.well-known/apple-app-site-association` with HTTP 200 and `Content-Type: application/json`. Use the final HTTPS host directly. The template names `org.tolglow.ios.Clip` under `appclips.apps` and gives the parent `org.tolglow.ios` a Universal Link for `/audience/*`. Adjust both identifiers if you change them in `native/project.yml`.
4. Configure the App Clip experience for the matching audience URL prefix in App Store Connect. Scan a current stage QR with the parent absent, then repeat with the parent installed. Verify the expected stage, participant consent screen and leave action on the actual phone. [Associate your Clip with a website](https://developer.apple.com/documentation/appclip/associating-your-app-clip-with-your-website) · [Associated Domains](https://developer.apple.com/documentation/xcode/supporting-associated-domains) · [Configure the launch experience](https://developer.apple.com/documentation/appclip/configuring-the-launch-experience-of-your-app-clip)

## Install the simulator artifact

The Packaged Apple Studio workflow builds the Release iPhone/iPad companion with its embedded Watch app and Clip. Download `Tolglow-Companion-simulator`, unzip the artifact, and run these commands on a Mac with a booted iPhone or iPad simulator matching the listed architecture.

```sh
ditto -x -k Tolglow-Companion-simulator.zip .
xcrun simctl install booted TolglowPhone.app
xcrun simctl launch booted org.tolglow.ios
```

The adjacent `simulator-products.json` measures unpacked file bytes inside each bundle and records the platform, device family and Mach-O architectures. The parent size includes the Clip and Watch. Use a signed device archive to measure eligibility for physical QR invocation.

## Measure the signed device variants

Run this on a Mac with your signing team, the required capabilities, and provisioning for all three products. Supply your own Team ID and domain.

```sh
cd native
xcodegen generate
xcodebuild -project Tolglow.xcodeproj -scheme TolglowPhone -configuration Release \
  -destination 'generic/platform=iOS' -archivePath build/Tolglow.xcarchive \
  DEVELOPMENT_TEAM="$TEAM_ID" AUDIENCE_DOMAIN="$AUDIENCE_DOMAIN" archive
```

Export locally using Xcode’s **Release Testing** distribution method and **All compatible device variants**. For an equivalent scripted export, create an export options plist with automatic signing, your team, and all variants.

```sh
plutil -create xml1 build/ExportOptions.plist
plutil -insert method -string release-testing build/ExportOptions.plist
plutil -insert signingStyle -string automatic build/ExportOptions.plist
plutil -insert teamID -string "$TEAM_ID" build/ExportOptions.plist
plutil -insert thinning -string '<thin-for-all-variants>' build/ExportOptions.plist
xcodebuild -exportArchive -archivePath build/Tolglow.xcarchive \
  -exportPath build/device-export -exportOptionsPlist build/ExportOptions.plist
cat 'build/device-export/App Thinning Size Report.txt'
Packaging/device-clip-sizes build/device-export
```

Keep the report and device variant with the release. Pair the Xcode report with the per-IPA Clip byte counts from the last command. It locates the signed Clip by its bundle identifier, including a Clip embedded in its parent, and counts its uncompressed files in each device variant. Compare the **uncompressed** Clip size against Apple's physical invocation limit for your minimum supported iOS. The expanded allowance for digital-only launches has different eligibility. Validate a real QR invocation once the signed experience is available. [Apple’s size measurement instructions](https://developer.apple.com/documentation/xcode/reducing-your-app-s-size) · [App Clip size and invocation requirements](https://developer.apple.com/documentation/appclip/choosing-the-right-functionality-for-your-app-clip)
