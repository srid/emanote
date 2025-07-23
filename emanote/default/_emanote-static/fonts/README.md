# MavenPro Font Files

This directory contains the MavenPro font files used by Emanote for consistent typography throughout the site.

## Font Details

- **Font Family**: Maven Pro
- **Source**: Google Fonts
- **Weights Included**: 400 (Regular), 500 (Medium), 600 (Semi-bold), 700 (Bold)
- **Format**: TrueType (.ttf)
- **License**: Open Font License (OFL)

## Current Font Files

- `maven-pro-400.ttf` - Regular weight
- `maven-pro-500.ttf` - Medium weight  
- `maven-pro-600.ttf` - Semi-bold weight
- `maven-pro-700.ttf` - Bold weight
- `maven-pro.css` - Font-face declarations

## How These Files Were Downloaded

The font files were downloaded from Google Fonts on July 23, 2025:

1. First, get the Google Fonts CSS to see the font URLs:
   ```bash
   curl -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36" \
     "https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;600;700&display=swap"
   ```

2. Download each font file individually:
   ```bash
   curl -o "maven-pro-400.ttf" "https://fonts.gstatic.com/s/mavenpro/v39/7Auup_AqnyWWAxW2Wk3swUz56MS91Eww8SX25nA.ttf"
   curl -o "maven-pro-500.ttf" "https://fonts.gstatic.com/s/mavenpro/v39/7Auup_AqnyWWAxW2Wk3swUz56MS91Eww8Rf25nA.ttf"
   curl -o "maven-pro-600.ttf" "https://fonts.gstatic.com/s/mavenpro/v39/7Auup_AqnyWWAxW2Wk3swUz56MS91Eww8fvx5nA.ttf"
   curl -o "maven-pro-700.ttf" "https://fonts.gstatic.com/s/mavenpro/v39/7Auup_AqnyWWAxW2Wk3swUz56MS91Eww8cLx5nA.ttf"
   ```

## How to Update These Fonts

To update to newer versions of MavenPro in the future:

1. Get the CSS by querying Google Fonts:
   ```bash
   curl -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36" \
     "https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;600;700&display=swap"
   ```
2. Extract the font URLs from the CSS response
3. Download each font file using curl (replace URLs with the new ones from step 2)
4. Update the `maven-pro.css` file if the font paths change

## Integration

The fonts are integrated into Emanote via:

1. **CSS Import**: `maven-pro.css` is imported in `/templates/styles.tpl`
2. **Body Font**: The body element uses `font-family: 'Maven Pro', ...` as the primary font
3. **Fallbacks**: System fonts are provided as fallbacks for better compatibility

## Notes

- The font files are self-hosted to keep Emanote sites self-contained without external dependencies
- All font weights use `font-display: swap` for better loading performance
- The font files are relatively small (~55KB each) for good performance
- MavenPro is licensed under the Open Font License, making it free for any use
