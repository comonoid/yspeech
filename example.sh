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

# ── Запуск ───────────────────────────────────────────────────────
cabal run yspeech -- \
  -i "$INPUT" \
  -o "$OUTPUT" \
  --language ru-RU \
  -v

echo "Результат: $OUTPUT"
