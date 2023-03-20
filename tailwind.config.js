/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["src/**/*.clj*", "src/**/*.html", "src/**/*.css"],
  theme: {
    extend: {
      fontFamily: {
        'sans': ['senor-saturno', 'ui-sans-serif', 'system-ui'],
      }  
    },
  },
  plugins: [],
}
