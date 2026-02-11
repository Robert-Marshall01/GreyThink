/** @type {import('tailwindcss').Config} */
export default {
    content: ['./index.html', './src/**/*.{js,ts,jsx,tsx}'],
    theme: {
        extend: {
            colors: {
                grey: {
                    50: '#f8f9fa',
                    100: '#f1f3f5',
                    200: '#e9ecef',
                    300: '#dee2e6',
                    400: '#ced4da',
                    500: '#adb5bd',
                    600: '#868e96',
                    700: '#495057',
                    800: '#343a40',
                    900: '#212529',
                    950: '#16191d',
                },
                brand: {
                    50: '#e8f4fd',
                    100: '#bde0f9',
                    200: '#91ccf5',
                    300: '#66b8f1',
                    400: '#3aa4ed',
                    500: '#1a8fe0',
                    600: '#1577bc',
                    700: '#105f98',
                    800: '#0b4774',
                    900: '#062f50',
                },
                accent: {
                    emerald: '#10b981',
                    amber: '#f59e0b',
                    rose: '#f43f5e',
                    violet: '#8b5cf6',
                    cyan: '#06b6d4',
                },
            },
            fontFamily: {
                sans: ['Inter', 'system-ui', '-apple-system', 'sans-serif'],
                mono: ['JetBrains Mono', 'Fira Code', 'monospace'],
            },
            boxShadow: {
                'card': '0 1px 3px 0 rgba(0, 0, 0, 0.06), 0 1px 2px -1px rgba(0, 0, 0, 0.06)',
                'card-hover': '0 4px 6px -1px rgba(0, 0, 0, 0.08), 0 2px 4px -2px rgba(0, 0, 0, 0.06)',
                'panel': '0 10px 15px -3px rgba(0, 0, 0, 0.08), 0 4px 6px -4px rgba(0, 0, 0, 0.04)',
            },
        },
    },
    plugins: [],
};
