#!/usr/bin/env bash
set -euo pipefail

# ── Yandex Cloud credentials ────────────────────────────────────
# API-ключ сервисного аккаунта (или IAM-токен, см. ниже)
#export YANDEX_API_KEY=""
# Идентификатор каталога в Yandex Cloud
#export YANDEX_FOLDER_ID=""

# Если вместо API-ключа используется IAM-токен, закомментируйте
# YANDEX_API_KEY и раскомментируйте строку ниже:
# export YANDEX_IAM_TOKEN="t1.9euelg..."

# ── Параметры ────────────────────────────────────────────────────
INPUT="${1:?Использование: $0 <аудиофайл.mp3>}"
OUTPUT="$(basename "${INPUT%.*}").txt"

# Работаем из каталога скрипта, чтобы cabal нашёл проект и shell.nix
cd "$(dirname "${BASH_SOURCE[0]}")"

# ── Запуск ───────────────────────────────────────────────────────
# На NixOS cabal не найдёт C-библиотеку zlib без окружения из shell.nix.
# Если мы ещё не внутри nix-shell и shell.nix есть — заходим в него.
CABAL_CMD=(cabal run yspeech -- \
  -i "$INPUT" \
  -o "$OUTPUT" \
  --language ru-RU \
  -v)

if [[ -z "${IN_NIX_SHELL:-}" && -f shell.nix ]] && command -v nix-shell >/dev/null 2>&1; then
  nix-shell --run "$(printf '%q ' "${CABAL_CMD[@]}")"
else
  "${CABAL_CMD[@]}"
fi

echo "Результат: $OUTPUT"
