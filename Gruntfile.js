module.exports = function(grunt) {

  grunt.initConfig({
    elm: {
      compile: {
        files: {
          "tictactoe.js": ["tic-tac-toe.elm"]
        }
      }
    },
    watch: {
      elm: {
        files: ["tic-tac-toe.elm"],
        tasks: ["elm"]
      }
    },
    clean: ["elm-stuff/build-artifacts"]
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-elm');

  grunt.registerTask('default', ['elm']);

};

