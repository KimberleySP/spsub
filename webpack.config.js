const path = require('path');
const webpack = require('webpack');
const { VueLoaderPlugin } = require('vue-loader');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');
const CompressionPlugin = require('compression-webpack-plugin');
const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer');

const isProd = process.env.NODE_ENV === 'production';
const isDev = !isProd;

const cssLoaders = (extra) => {
  const loaders = [
    isDev ? 'style-loader' : MiniCssExtractPlugin.loader,
    {
      loader: 'css-loader',
      options: {
        sourceMap: isDev,
        importLoaders: extra ? 2 : 1,
        modules: {
          localIdentName: isDev
            ? '[name]__[local]___[hash:base64:5]'
            : '[hash:base64:8]'
        }
      }
    },
    {
      loader: 'postcss-loader',
      options: {
        sourceMap: isDev,
        postcssOptions: {
          plugins: [
            ['postcss-preset-env', { browsers: 'last 2 versions' }],
            isProd && ['cssnano', { preset: 'advanced' }]
          ].filter(Boolean)
        }
      }
    }
  ];

  if (extra) {
    loaders.push({
      loader: extra,
      options: {
        sourceMap: isDev,
        additionalData: extra === 'sass-loader' 
          ? `@import "@/styles/variables.scss";`
          : undefined
      }
    });
  }

  return loaders;
};

module.exports = {
  mode: isProd ? 'production' : 'development',
  entry: {
    app: './src/main.ts',
    vendor: ['vue', 'vue-router', 'vuex', 'axios']
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: isProd ? 'js/[name].[contenthash:8].js' : 'js/[name].js',
    chunkFilename: isProd ? 'js/[name].[contenthash:8].chunk.js' : 'js/[name].chunk.js',
    assetModuleFilename: 'assets/[hash][ext][query]',
    clean: true
  },
  ...(about 150 more lines)
}; 