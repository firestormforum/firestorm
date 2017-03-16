# FirestormNotifier

This is responsible for notifying users when stuff happens. By default we use
Mailgun via Bamboo to do this over email. It'll do more later. It's a GenStage
consumer of the `FirestormData.Events` and it sends emails when it sees events
come through.
