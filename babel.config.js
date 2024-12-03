module.exports = {
  presets: [
    ['@babel/preset-env', {
      modules: false,
      useBuiltIns: 'usage',
      corejs: { version: 3.8, proposals: true },
      targets: {
        browsers: [
          'last 2 Chrome versions',
          'last 2 Firefox versions',
          'last 2 Safari versions',
          'last 2 Edge versions',
          'not IE 11',
          'not dead'
        ]
      }
    }],
    '@babel/preset-typescript',
    ['@vue/babel-preset-jsx', {
      compositionAPI: true,
      injectH: true
    }]
  ],
  plugins: [
    ['@babel/plugin-transform-runtime', {
      corejs: 3,
      helpers: true,
      regenerator: true,
      useESModules: true,
    }],
    ['@babel/plugin-proposal-decorators', { legacy: true }],
    ['@babel/plugin-proposal-class-properties', { loose: true }],
    ['@babel/plugin-proposal-private-methods', { loose: true }],
    ['@babel/plugin-proposal-private-property-in-object', { loose: true }],
    '@babel/plugin-proposal-nullish-coalescing-operator',
    '@babel/plugin-proposal-optional-chaining',
    '@babel/plugin-syntax-dynamic-import',
    'babel-plugin-const-enum'
  ],
  env: {
    test: {
      presets: [['@babel/preset-env', { targets: { node: 'current' } }]],
      plugins: ['babel-plugin-dynamic-import-node']
    },
    production: {
      plugins: [
        ['transform-remove-console', { exclude: ['error', 'warn'] }],
        'transform-remove-debugger'
      ]
    }
  }
}; 