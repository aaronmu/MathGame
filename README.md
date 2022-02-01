# Lit.MathGame

https://aaronmu.github.io/MathGame/

[Fable.Lit](https://github.com/fable-compiler/Fable.Lit) app. To start a development server run:

```
npm install && npm start
```

Other commands:

```bash
npm run build   # Build optimized site for deployment and put in dist/
npm run publish # Build and publish to github pages
```

## Vite.js repository structure conventions

- Put static files in `public/` folder
- Put `index.html` in app root (next to `package.json`)
- Add a reference to the entry JS file (relative path is important):

```html
<script type="module" src="./build/client/App.js"></script>
```
