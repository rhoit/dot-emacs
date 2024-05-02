#!/usr/bin/bash

set -e
set -v


FILE=/usr/local/bin/emacs
cat > $FILE <<EOF
LSP_USE_PLISTS=true exec /usr/bin/emacs "\$@"
EOF
chmod +x $FILE


FILE=/usr/local/bin/emacsclient
cat > $FILE <<EOF
exec /usr/bin/emacsclient --no-wait --alternate-editor=/usr/local/bin/emacs "\$@"
EOF
chmod +x $FILE
