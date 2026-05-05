const I18N_SCRIPT_ID = 'emanote-i18n';

let cachedSource = null;
let cachedTranslations = {};

export function text(key, fallback = '') {
  const value = translations()[key];
  return typeof value === 'string' && value.length > 0 ? value : fallback;
}

export function message(key, params = {}, fallback = '') {
  let template = text(key, fallback);
  for (const [name, value] of Object.entries(params)) {
    template = template.replaceAll('{' + name + '}', String(value));
  }
  return template;
}

function translations() {
  const source = document.getElementById(I18N_SCRIPT_ID)?.textContent || '{}';
  if (source === cachedSource) return cachedTranslations;

  cachedSource = source;
  try {
    const parsed = JSON.parse(source);
    cachedTranslations = parsed && typeof parsed === 'object' && !Array.isArray(parsed)
      ? parsed
      : {};
  } catch (err) {
    console.warn('[emanote] failed to parse localized UI strings; using inline fallbacks', err);
    cachedTranslations = {};
  }
  return cachedTranslations;
}
